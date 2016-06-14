package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.util.Arrays;
import java.util.BitSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3d;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector4f;

public class WormSegmentationWindowing extends WormSegmentation
{
	public static float[] robustHistogram(ModelImage image, Vector<Vector3f> dataPoints )
	{
		double min = image.getMin();
		double max = image.getMax();    	    	
		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;    	

		// Calculate the histogram:
		HashMap<Float, Integer> map = new HashMap<Float, Integer>();
		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					float value = image.getFloat(x,y,z);
					int count = 0;
					if ( map.containsKey(value) )
					{
						count = map.get(value);
					}
					count++;
					map.put( value, count );
				}
			}
		}

		int totalCount = dimX * dimY * dimZ;
		int count_2P = (int) (0.02 * totalCount);
		int count_985P = (int) (0.985 * totalCount);
		int count_995P = (int) (0.995 * totalCount);
		int count_999P = (int) (0.999 * totalCount);

		// Sort the Histogram bins:
		Set<Float> keySet = map.keySet();
		Iterator<Float> keyIterator = keySet.iterator();
		float[] keyArray = new float[keySet.size()];
		int count = 0;
		while ( keyIterator.hasNext() )
		{
			float value = keyIterator.next();
			keyArray[count++] = value;
		}

		Arrays.sort(keyArray);
		int[] counts = new int[keyArray.length];
		int runningCount = 0;
		boolean minFound = false;
		boolean maxLRFound = false;
		boolean found985 = false;
		boolean found995 = false;
		float min_2P = (float) image.getMin();
		float max_LR = (float) image.getMax();
		float lr985 = min_2P;
		float lr995 = max_LR;
		for ( int i = 0; i < counts.length; i++ )
		{
			counts[i] = map.get( keyArray[i] );
			runningCount += counts[i];

			if ( !minFound && (runningCount >= count_2P) )
			{
				min_2P = keyArray[i];
				minFound = true;
			}
			if ( !found985 && (runningCount >= count_985P) )
			{
				lr985 = keyArray[i];
				found985 = true;
			}
			if ( !found995 && (runningCount >= count_995P) )
			{
				lr995 = keyArray[i];
				found995 = true;
			}
			if ( !maxLRFound && (runningCount >= count_999P) )
			{
				max_LR = keyArray[i];
				maxLRFound = true;
			}
		}

		if ( dataPoints != null )
		{
			for ( int z = 0; z < dimZ; z++ )
			{
				for ( int y = 0; y < dimY; y++ )
				{
					for ( int x = 0; x < dimX; x++ )
					{
						float value = image.getFloat(x,y,z);
						if ( value < min_2P )
						{
							value = min_2P;
						}
						else if ( value > max_LR )
						{
							value = max_LR;    					
						}
						if ( value >= (.90f*max_LR) )
						{
							dataPoints.add( new Vector3f( x, y, z ) );
						}
						//    				image.set(x,y,z,value);
					}
				}
			}
		}
		//		image.calcMinMax();
		return new float[]{lr985,lr995};
	}

	public static Vector<Vector3f> seamCellSegmentation( ModelImage image, float minValue, float maxValue )
	{
		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1; 
		Vector3f min = new Vector3f(dimX, dimY, dimZ);		
		Vector3f max = new Vector3f(0,0,0);

		// find the left right markers based on the threshold values and clustering...
		Vector<Vector3f> segmentation = new Vector<Vector3f>();
		Vector<Vector3d> centers = findMarkers(image, null, maxValue, (float)image.getMax(), 15, 30, min, max);
		for ( int i = 0; i < centers.size(); i++ )
		{
			Vector3d center = centers.elementAt(i);
			segmentation.add( new Vector3f( (float)center.X, (float)center.Y, (float)center.Z ) );
		}
		return segmentation;
	}

	public static Vector<Vector3f> nucleiSegmentation( ModelImage image, float minValue, int minDiameter, int maxDiameter )
	{
		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1; 
		Vector3f min = new Vector3f(dimX, dimY, dimZ);		
		Vector3f max = new Vector3f(0,0,0);

		// find the left right markers based on the threshold values and clustering...
		Vector<Vector3f> segmentation = new Vector<Vector3f>();
		Vector<Vector3d> centers = findMarkers(image, null, minValue, (float)image.getMax(), minDiameter, maxDiameter, min, max);
		for ( int i = 0; i < centers.size(); i++ )
		{
			Vector3d center = centers.elementAt(i);
			segmentation.add( new Vector3f( (float)center.X, (float)center.Y, (float)center.Z ) );
		}    	
		return segmentation;
	}

	private static Vector<Vector3d> findMarkers( ModelImage image, BitSet leftRightMask, float intensityMin, float intensityMax, int diameter, int maxDiameter, Vector3f min, Vector3f max )
	{
		int cubeSize = diameter;
		int cubeHalf = cubeSize/2;    	

		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;    

		Vector3f temp = new Vector3f();
		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					float value = image.getFloat(x,y,z);
					if ( (value > intensityMin) && (value <= intensityMax) )
					{
						temp.set(x,y,z);
						min.min( temp );
						max.max( temp );
					}
				}
			}
		}
		//    	System.err.println( "Min bounding box = " + min );
		//    	System.err.println( "Max bounding box = " + max );

		if ( min.X < cubeHalf ) min.X = cubeHalf;
		if ( min.Y < cubeHalf ) min.Y = cubeHalf;
		if ( min.Z < cubeHalf ) min.Z = cubeHalf;

		if ( max.X > (dimX - cubeHalf) ) max.X = (dimX - cubeHalf);
		if ( max.Y > (dimY - cubeHalf) ) max.Y = (dimY - cubeHalf);
		if ( max.Z > (dimZ - cubeHalf) ) max.Z = (dimZ - cubeHalf);

		//    	System.err.println( "Min bounding box = " + min );
		//    	System.err.println( "Max bounding box = " + max );


		//    	Vector<Vector2d> cubeIntensities = new Vector<Vector2d>();
		Vector<Vector3f> cubeCenters = new Vector<Vector3f>();

		//    	double minVar = Double.MAX_VALUE;
		//    	double maxVar = -Double.MAX_VALUE;

		Vector3f centerP = new Vector3f();
		Vector3f testP = new Vector3f();
		// Shifting 5x5x5 voxel cube through the volume, calculating the mean intesity and variance:
		for ( int z = (int) min.Z; z < max.Z; z++ )
		{
			for ( int y = (int) min.Y; y < max.Y; y++ )
			{
				for ( int x = (int) min.X; x < max.X; x++ )
				{
					float value = image.getFloat(x,y,z);

					if ( (value > intensityMin) && (value <= intensityMax) )
					{
						boolean solid = true;
						centerP.set(x,y,z);

						if ( leftRightMask != null )
						{
							int index = (z * dimX * dimY + y * dimX + x);	
							if (leftRightMask.get(index) )
							{
								continue;
							}
						}

						int count = 0;
						double meanIntensity = 0;
						for ( int z2 = z - cubeHalf; z2 < (z+cubeHalf); z2++ )
						{
							for ( int y2 = y - cubeHalf; y2 < (y+cubeHalf); y2++ )
							{
								for ( int x2 = x - cubeHalf; x2 < (x+cubeHalf); x2++ )
								{
									if ( (x2 < dimX) && (y2 < dimY) && (z2 < dimZ) )
									{
										testP.set(x2,y2,z2);
										if ( centerP.distance(testP) <= cubeHalf )
										{
											value = image.getFloat(x2,y2,z2);
											meanIntensity += value;
											count++;

											if ( value < (intensityMin / 2) )
											{
												solid = false;
											}
										}
									}
								}
							}    					
						}
						meanIntensity /= count;

						if ( solid && meanIntensity >= intensityMin )
						{
							Vector3f center = new Vector3f( x, y, z );
							cubeCenters.add(center);
						}
					}
				}
			}
		}    	

		Vector<Vector3d> clusterCenters = makeClusters( cubeCenters, cubeSize, maxDiameter );
		//    	Vector<VOIContour> clusterList = makeClusters( cubeCenters, cubeSize, maxDiameter );
		//    	
		//    	VOIContour centerPoints = new VOIContour(false);
		//    	while ( clusterList.size() > 0 )
		//    	{
		//    		int maxSize = -1;
		//    		int maxIndex = -1;
		//    		for ( int i = 0; i < clusterList.size(); i++ )
		//    		{
		//    			VOIContour cluster = clusterList.elementAt(i);
		//    			if ( cluster.size() > maxSize )
		//    			{
		//    				maxSize = cluster.size();
		//    				maxIndex = i;
		//    			}
		//    		}
		//    		if ( maxIndex >= 0 && maxIndex < clusterList.size() )
		//    		{
		//    			VOIContour cluster = clusterList.elementAt(maxIndex);
		////    			System.err.println( "   cluster = " + cluster.size() );
		//    			Vector3f center = new Vector3f();
		//    			for ( int j = 0; j < cluster.size(); j++ )
		//    			{
		//    				center.add( cluster.elementAt(j) );
		//    			}
		//    			center.scale( 1f/cluster.size() );
		//    			centerPoints.add(center);
		//    			clusterList.remove(maxIndex);
		//    		}
		//    	}

		if ( diameter != maxDiameter )
		{
			for ( int i = clusterCenters.size() - 1; i >= 0; i-- )
			{
				for ( int j = i-1; j >= 0; j-- )
				{
					if ( clusterCenters.elementAt(i).distance( clusterCenters.elementAt(j) ) < maxDiameter )
					{
						if ( mergePoints( image, intensityMin, clusterCenters.elementAt(i), clusterCenters.elementAt(j) ) )
						{
							//    					System.err.println( "Merging points" );
							Vector3d pt = clusterCenters.remove(i);
							clusterCenters.elementAt(j).add(pt);
							clusterCenters.elementAt(j).scale(0.5f);    			
							break;
						}
					}
				}
			}
		}
		for ( int i = 0; i < clusterCenters.size(); i++ )
		{
			clusterCenters.elementAt(i).X = Math.round(clusterCenters.elementAt(i).X);
			clusterCenters.elementAt(i).Y = Math.round(clusterCenters.elementAt(i).Y);
			clusterCenters.elementAt(i).Z = Math.round(clusterCenters.elementAt(i).Z);
		}
		for ( int i = clusterCenters.size()-1; i >= 0; i-- )
		{
			for ( int j = i-1; j >= 0; j-- )
			{
				if ( clusterCenters.elementAt(i).isEqual( clusterCenters.elementAt(j) ) )
				{
					clusterCenters.remove(i);
					break;
				}
			}
		}
		cubeCenters.clear();
		cubeCenters = null;
		return clusterCenters;
	}

	private static Vector<Vector3d> makeClusters( Vector<Vector3f> cubeCenters, int diameter, int maxDiameter )
	{

		Vector<VOIContour> clusterList = new Vector<VOIContour>();
		Vector<Vector3d> clusterSum = new Vector<Vector3d>();
		for ( int i = 0; i < cubeCenters.size(); i++ )
		{
			Vector3f p1 = cubeCenters.elementAt(i);
			if ( clusterList.size() == 0 )
			{
				VOIContour cluster = new VOIContour( false );
				cluster.add(p1);
				clusterList.add( cluster );
				clusterSum.add( new Vector3d(p1.X, p1.Y, p1.Z) );
			}
			else
			{
				float minDist = Float.MAX_VALUE;
				int minClusterIndex = -1;
				for ( int j = 0; j < clusterList.size(); j++ )
				{
					VOIContour cluster = clusterList.elementAt(j);
					Vector3d sum = clusterSum.elementAt(j);
					double scale = 1.0/cluster.size();
					Vector3f avg = new Vector3f( (float)(sum.X * scale), (float)(sum.Y * scale), (float)(sum.Z * scale) );
					float distance = p1.distance( avg );
					if ( distance < minDist )
					{
						minDist = distance;
						minClusterIndex = j;
					}
				}
				if ( (minClusterIndex == -1) || (minDist > diameter) )
				{
					VOIContour cluster = new VOIContour( false );
					cluster.add(p1);
					clusterList.add( cluster );    		
					clusterSum.add( new Vector3d(p1.X, p1.Y, p1.Z) );		
				}
				else
				{
					clusterList.elementAt(minClusterIndex).add(p1);
					clusterSum.elementAt(minClusterIndex).add( new Vector3d(p1.X, p1.Y, p1.Z) );
				}
			}
		}	
		for ( int i = 0; i < clusterList.size(); i++ )
		{
			VOIContour cluster = clusterList.elementAt(i);
			Vector3d sum = clusterSum.elementAt(i);
			double scale = 1.0/cluster.size();
			Vector3d avg = new Vector3d( (sum.X * scale), (sum.Y * scale), (sum.Z * scale) );
			sum.copy( avg );
		}
		clusterList.clear();
		clusterList = null;
		return clusterSum;    	
	}

	private static boolean mergePoints( ModelImage image, float intensityMin, Vector3d p1, Vector3d p2 )
	{    	
		Vector3d dir = Vector3d.sub( p2, p1 );
		dir.normalize();
		double steps = p1.distance(p2);
		Vector3d start = new Vector3d(p1);
		for ( int i = 0; i < steps; i++ )
		{
			start.add(dir);
			if ( image.getFloatTriLinearBounds( (float)start.X, (float)start.Y, (float)start.Z ) < (.95*intensityMin) )
			{
				return false;
			}
		}
		return true;
	}



	public static VOI findMarkersA( ModelImage image, float intensityMin, float intensityMax, int minRadius, int maxRadius )
	{

		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1; 
		BitSet mask = new BitSet(dimX*dimY*dimZ);

		VOI markers = new VOI( (short) 0, "all markers", VOI.POLYLINE, 1 );
		Vector<Vector3f> allSizes = findMarkersA(image, mask, intensityMin, intensityMax, maxRadius );
		System.err.println( "num markers " + allSizes.size() + " radius " + maxRadius + " " + mask.cardinality() );
		VOIContour c = new VOIContour(false);
		c.addAll(allSizes);
		markers.getCurves().add(c);
		for ( int i = maxRadius - 1; i >= minRadius; i-- )
		{
			allSizes = findMarkersA(image, mask, intensityMin, intensityMax, i);
			System.err.println( "num markers " + allSizes.size() + " radius " + i + " " + mask.cardinality() );

			c = new VOIContour(false);
			c.addAll(allSizes);
			markers.getCurves().add(c);
		}
		return markers;
	}

	private static Vector<Vector3f> findMarkersA( ModelImage image, BitSet mask, float intensityMin, float intensityMax, int radius )
	{    	
		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;    

		Vector3f min = new Vector3f(dimX,dimY,dimZ);
		Vector3f max = new Vector3f(-dimX,-dimY,-dimZ);
		Vector3f temp = new Vector3f();
		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					float value = 0;
					if ( image.isColorImage() )
					{
						value = image.getFloatC(x, y, z, 1 ); 
					}
					else
					{
						value = image.getFloat(x,y,z);
					}
					if ( (value > intensityMin) && (value <= intensityMax) )
					{
						temp.set(x,y,z);
						min.min( temp );
						max.max( temp );
					}
				}
			}
		}

		if ( min.X < radius ) min.X = radius;
		if ( min.Y < radius ) min.Y = radius;
		if ( min.Z < radius ) min.Z = radius;

		if ( max.X > (dimX - 1 - radius) ) max.X = (dimX - 1 - radius);
		if ( max.Y > (dimY - 1 - radius) ) max.Y = (dimY - 1 - radius);
		if ( max.Z > (dimZ - 1 - radius) ) max.Z = (dimZ - 1 - radius);


		Vector<Vector3f> cubeCenters = new Vector<Vector3f>();
		Vector<Double> cubeLikelihood = new Vector<Double>();

		Vector3f centerP = new Vector3f();
		Vector3f testP = new Vector3f();


		for ( int z = (int) min.Z; z < max.Z; z++ )
		{
			for ( int y = (int) min.Y; y < max.Y; y++ )
			{
				for ( int x = (int) min.X; x < max.X; x++ )
				{
					float centerValue = 0;
					if ( image.isColorImage() )
					{
						centerValue = image.getFloatC(x, y, z, 1 ); 
					}
					else
					{
						centerValue = image.getFloat(x,y,z);
					}

					if ( (centerValue > intensityMin) && (centerValue <= intensityMax) )
					{
						int index = (z * dimX * dimY + y * dimX + x);
						if ( mask.get(index) )
						{
							continue;
						}


						boolean solid = true;
						centerP.set(x,y,z);

						int insideCount = 0;
						double insideAverage = 0;
						int outsideCount = 0;
						double outsideAverage = 0;
						for ( int z2 = z - radius; z2 < (z+radius); z2++ )
						{
							for ( int y2 = y - radius; y2 < (y+radius); y2++ )
							{
								for ( int x2 = x - radius; x2 < (x+radius); x2++ )
								{
									if ( (x2 < dimX) && (y2 < dimY) && (z2 < dimZ) )
									{
										testP.set(x2,y2,z2);
										if ( centerP.distance(testP) <= radius )
										{
											float value = 0;
											if ( image.isColorImage() )
											{
												value = image.getFloatC(x2, y2, z2, 1 ); 
											}
											else
											{
												value = image.getFloat(x2,y2,z2);
											}
											insideAverage += value;
											insideCount++;

											if ( value < (centerValue / 2) )
											{
												solid = false;
											}
										}
										else
										{
											float value = 0;
											if ( image.isColorImage() )
											{
												value = image.getFloatC(x2, y2, z2, 1 ); 
											}
											else
											{
												value = image.getFloat(x2,y2,z2);
											}
											outsideAverage += value;
											outsideCount++;
										}
									}
								}
							}    					
						}
						insideAverage /= insideCount;
						outsideAverage /= outsideCount;

						if ( solid && (outsideAverage <= 0.5*insideAverage) )
						{
							Vector3f center = new Vector3f( x, y, z );
							cubeCenters.add(center);

							double likelihood = 0;
							for ( int z2 = z - radius; z2 < (z+radius); z2++ )
							{
								for ( int y2 = y - radius; y2 < (y+radius); y2++ )
								{
									for ( int x2 = x - radius; x2 < (x+radius); x2++ )
									{
										if ( (x2 < dimX) && (y2 < dimY) && (z2 < dimZ) )
										{
											testP.set(x2,y2,z2);
											if ( centerP.distance(testP) <= radius )
											{
												index = (z2 * dimX * dimY + y2 * dimX + x2);
												mask.set(index);


												float value = 0;
												if ( image.isColorImage() )
												{
													value = image.getFloatC(x2, y2, z2, 1 ); 
													likelihood += (1 - (image.getMaxR() - value)/(float)image.getMaxR());
												}
												else
												{
													value = image.getFloat(x2,y2,z2);
													likelihood += (1 - (image.getMax() - value)/image.getMax());
												}
											}
										}
									}
								}    					
							}
							cubeLikelihood.add(new Double(likelihood));
						}
					}
				}
			}
		} 
		//    	for ( int i = cubeCenters.size() - 1; i >= 0; i-- )
		//    	{
		//    		System.err.println( i + " " + cubeLikelihood.elementAt(i) );
		//    	}
		//		System.err.println( cubeCenters.size() );
		return cubeCenters;
	}

	public static Vector<Vector3f> findMarkers( ModelImage image, float intensityMin, float intensityMax, int minRadius, int maxRadius )
	{
		Vector<Vector3f> pts = initFindMarkers(image, intensityMin, intensityMax);
//		pts = new Vector<Vector3f>();
//		pts.add(new Vector3f(153, 57, 150));
//		pts.add(new Vector3f(167, 45, 150));
//		pts.add(new Vector3f(114, 80, 151));
//		pts.add(new Vector3f(102, 106, 183));
		//		if ( true )
		//		{
		//			VOI markers = new VOI( (short) 0, "all markers", VOI.POLYLINE, 1 );
		//			VOIContour c = new VOIContour(false);
		//			c.addAll(pts);
		//			markers.getCurves().add(c);
		//			return markers;
		//		}

		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1; 
		BitSet mask = new BitSet(dimX*dimY*dimZ);
		ModelImage results = new ModelImage( ModelStorageBase.ARGB, image.getExtents(), "results" );

//		VOI markers = new VOI( (short) 0, "all markers", VOI.POLYLINE, 1 );
		Vector<Vector3f> allSizes = findMarkers(image, results, pts, mask, intensityMin, intensityMax, minRadius, maxRadius );
		System.err.println( "num markers " + allSizes.size() + " radius " + maxRadius + " " + mask.cardinality() );
//		VOIContour c = new VOIContour(false);
//		c.addAll(allSizes);
//		markers.getCurves().add(c);
		//		for ( int i = maxRadius - 1; i >= minRadius; i-- )
		//		{
		//			allSizes = findMarkers(image, pts, mask, intensityMin, intensityMax, i);
		//			System.err.println( "num markers " + allSizes.size() + " radius " + i + " " + mask.cardinality() );
		//
		//			c = new VOIContour(false);
		//			c.addAll(allSizes);
		//			markers.getCurves().add(c);
		//		}
		
		results.calcMinMax();
		new ViewJFrameImage(results);
//		return markers;
		return allSizes;
	}



	private static Vector<Vector3f> findMarkers( ModelImage image, ModelImage results, Vector<Vector3f> pts, BitSet mask, float intensityMin, float intensityMax, int minRadius, int maxRadius )
	{    	
		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;    

		Vector<Vector3f> cubeCenters = new Vector<Vector3f>();

		Vector3f centerP = new Vector3f();
		Vector3f testP = new Vector3f();

		for ( int i = pts.size() -1; i >= 0; i-- )
		{
			centerP = pts.elementAt(i);
			int index = (((int)centerP.Z) * dimX * dimY + ((int)centerP.Y) * dimX + ((int)centerP.X));
			if ( mask.get(index) )
			{
				continue;
			}

			boolean foundInside = false;
			boolean foundOutside = false;
			int outR = -1;
			int inR = -1;
			for ( int radius = minRadius; radius <= (maxRadius+1); radius++ )
			{
				int startZ = (int)(centerP.Z - radius);
				int stopZ  = (int)(centerP.Z + radius);
				int startY = (int)(centerP.Y - radius);
				int stopY  = (int)(centerP.Y + radius);
				int startX = (int)(centerP.X - radius);
				int stopX  = (int)(centerP.X + radius);
				if ( (startZ < 0) || (startY < 0) || (startX < 0) ||
						(stopZ > dimZ) || (stopY > dimY) || (stopX > dimX) )
				{
					continue;
				}
				
				float centerValue = 0;
				if ( image.isColorImage() )
				{
					centerValue = image.getFloatC((int)centerP.X, (int)centerP.Y, (int)centerP.Z, 1 ); 
				}
				else
				{
					centerValue = image.getFloat((int)centerP.X, (int)centerP.Y, (int)centerP.Z);
				}

				int insideCount = 0;
				double insideAverage = 0;
				int outsideCount = 0;
				double outsideAverage = 0;
				
				boolean insideMask = false;
				for ( int z = startZ; z <= stopZ && !insideMask; z++ )
				{
					for ( int y = startY; y <= stopY && !insideMask; y++ )
					{
						for ( int x = startX; x <= stopX && !insideMask; x++ )
						{

							index = (z * dimX * dimY + y * dimX + x);
							if ( mask.get(index) )
							{
								insideMask = true;
								break;
							}
							
							float value = 0;
							if ( image.isColorImage() )
							{
								value = image.getFloatC(x, y, z, 1 ); 
							}
							else
							{
								value = image.getFloat(x, y, z);
							}

							testP.set(x,y,z);
							if ( centerP.distance(testP) <= radius )
							{
								insideAverage += value;
								insideCount++;
							}
							else
							{
								outsideAverage += value;
								outsideCount++;
							}
						}
					}
				}
				if ( insideMask )
				{
					break;
				}
				
				insideAverage /= insideCount;
				outsideAverage /= outsideCount;

				foundInside = foundInside || (insideAverage >= (0.75 *centerValue));
				foundOutside = foundOutside || (foundInside && (outsideAverage <= (0.65 * centerValue)));
				if ( foundInside && (inR == -1) )
				{
					inR = radius;
				}
				if ( foundOutside && (outR == -1) )
				{
					outR = radius;
				}
				if ( foundInside && foundOutside )
				{
//					System.err.println( i + " " + centerValue + " " + insideAverage + " " + outsideAverage + " " +
//							insideAverage/centerValue + " " + outsideAverage/centerValue + " " +
//							(insideAverage >= (0.75 *centerValue)) + " " + (outsideAverage <= (0.5 * centerValue)) );

					cubeCenters.add( pts.remove(i) );
					startZ = (int)(centerP.Z - outR);
					stopZ  = (int)(centerP.Z + outR);
					startY = (int)(centerP.Y - outR);
					stopY  = (int)(centerP.Y + outR);
					startX = (int)(centerP.X - outR);
					stopX  = (int)(centerP.X + outR);
					for ( int z = startZ; z <= stopZ; z++ )
					{
						for ( int y = startY; y <= stopY; y++ )
						{
							for ( int x = startX; x <= stopX; x++ )
							{
								testP.set(x,y,z);
								if ( centerP.distance(testP) <= outR )
								{
									index = (z * dimX * dimY + y * dimX + x);
									mask.set(index);
									results.setC(x,  y, z, 0, 255);
									results.setC(x,  y, z, 3, 255);
								}
								if ( centerP.distance(testP) <= inR )
								{
									results.setC(x,  y, z, 0, 255);
									results.setC(x,  y, z, 1, 255);
								}
//								if ( centerP.distance(testP) == outR )
//								{
//									results.set(x,  y, z, 1);
//								}
//								if ( centerP.distance(testP) == inR )
//								{
//									results.set(x,  y, z, 1);
//								}
							}
						}
					}
					break;
				}
			}
		}
		return cubeCenters;
	}

	private static Vector<Vector3f> initFindMarkers( ModelImage image, float intensityMin, float intensityMax )
	{    	
		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;    

		Vector<Vector4f> positions = new Vector<Vector4f>();
		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					float value = 0;
					if ( image.isColorImage() )
					{
						value = image.getFloatC(x, y, z, 1 ); 
					}
					else
					{
						value = image.getFloat(x,y,z);
					}
					if ( (value > intensityMin) && (value <= intensityMax) )
					{
						Vector4f pos = new Vector4f( value, x, y, z );
						positions.add(pos);
					}
				}
			}
		}

		Vector4f[] posArray = new Vector4f[positions.size()];
		for ( int i = positions.size() - 1; i >= 0; i-- )
		{
			posArray[i] = positions.remove(i);
		}
		Arrays.sort(posArray);

		Vector<Vector3f> sortedList = new Vector<Vector3f>();
		for ( int i = 0; i < posArray.length; i++ )
		{
//			System.err.println( i + " " + posArray[i] );
			sortedList.add(new Vector3f(posArray[i].Y, posArray[i].Z, posArray[i].W) );
		}
		positions = null;
		posArray = null;

		System.err.println( sortedList.size() );

		return sortedList;
	}


	public WormSegmentationWindowing() {}

}
