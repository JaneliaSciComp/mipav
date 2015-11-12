package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOIContour;

import java.util.Arrays;
import java.util.BitSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3d;
import WildMagic.LibFoundation.Mathematics.Vector3f;

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
		Vector<Vector3f> left_right_markers = findLeftRightMarkers(image, min, max, maxValue, (float)image.getMax());
		return left_right_markers;
	}

    private static Vector<Vector3f> findLeftRightMarkers(ModelImage image, Vector3f min, Vector3f max, float max_LR, float intensityMax)
    {

    	VOIContour centers = findMarkers(image, null, max_LR, intensityMax, 15, 30, min, max); 
    	Vector<Vector3f> left_right_markers = new Vector<Vector3f>();
		
    	for ( int i = 0; i < centers.size(); i++ )
    	{
    		Vector3f seed = centers.elementAt(i); 
    		left_right_markers.add(new Vector3f(seed));
    	}
    	return left_right_markers;
    }
    
    private static VOIContour findMarkers( ModelImage image, BitSet leftRightMask, float intensityMin, float intensityMax, int diameter, int maxDiameter, Vector3f min, Vector3f max )
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
    	
    	double minVar = Double.MAX_VALUE;
    	double maxVar = -Double.MAX_VALUE;
    	
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

    	Vector<VOIContour> clusterList = makeClusters( cubeCenters, cubeSize, maxDiameter );
    	
    	VOIContour centerPoints = new VOIContour(false);
    	while ( clusterList.size() > 0 )
    	{
    		int maxSize = -1;
    		int maxIndex = -1;
    		for ( int i = 0; i < clusterList.size(); i++ )
    		{
    			VOIContour cluster = clusterList.elementAt(i);
    			if ( cluster.size() > maxSize )
    			{
    				maxSize = cluster.size();
    				maxIndex = i;
    			}
    		}
    		if ( maxIndex >= 0 && maxIndex < clusterList.size() )
    		{
    			VOIContour cluster = clusterList.elementAt(maxIndex);
//    			System.err.println( "   cluster = " + cluster.size() );
    			Vector3f center = new Vector3f();
    			for ( int j = 0; j < cluster.size(); j++ )
    			{
    				center.add( cluster.elementAt(j) );
    			}
    			center.scale( 1f/cluster.size() );
    			centerPoints.add(center);
    			clusterList.remove(maxIndex);
    		}
    	}
    	
    	if ( diameter != maxDiameter )
    	{
    		for ( int i = centerPoints.size() - 1; i >= 0; i-- )
    		{
    			for ( int j = i-1; j >= 0; j-- )
    			{
    				if ( centerPoints.elementAt(i).distance( centerPoints.elementAt(j) ) < maxDiameter )
    				{
    					if ( mergePoints( image, intensityMin, centerPoints.elementAt(i), centerPoints.elementAt(j) ) )
    					{
    						//    					System.err.println( "Merging points" );
    						Vector3f pt = centerPoints.remove(i);
    						centerPoints.elementAt(j).add(pt);
    						centerPoints.elementAt(j).scale(0.5f);    			
    						break;
    					}
    				}
    			}
    		}
    	}
    	for ( int i = 0; i < centerPoints.size(); i++ )
    	{
    		centerPoints.elementAt(i).X = Math.round(centerPoints.elementAt(i).X);
    		centerPoints.elementAt(i).Y = Math.round(centerPoints.elementAt(i).Y);
    		centerPoints.elementAt(i).Z = Math.round(centerPoints.elementAt(i).Z);
    	}
    	for ( int i = centerPoints.size()-1; i >= 0; i-- )
    	{
    		for ( int j = i-1; j >= 0; j-- )
    		{
    			if ( centerPoints.elementAt(i).isEqual( centerPoints.elementAt(j) ) )
    			{
    				centerPoints.remove(i);
    				break;
    			}
    		}
    	}
    	cubeCenters.clear();
    	cubeCenters = null;
    	return centerPoints;
    }
    
    private static Vector<VOIContour> makeClusters( Vector<Vector3f> cubeCenters, int diameter, int maxDiameter )
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
    	clusterSum.clear();
    	clusterSum = null;
    	return clusterList;    	
    }
    
    private static boolean mergePoints( ModelImage image, float intensityMin, Vector3f p1, Vector3f p2 )
    {    	
    	Vector3f dir = Vector3f.sub( p2, p1 );
    	dir.normalize();
    	float steps = p1.distance(p2);
    	Vector3f start = new Vector3f(p1);
    	for ( int i = 0; i < steps; i++ )
    	{
    		start.add(dir);
    		if ( image.getFloatTriLinearBounds( start.X, start.Y, start.Z ) < (.95*intensityMin) )
    		{
    			return false;
    		}
    	}
    	return true;
    }
    
    public WormSegmentationWindowing() {}

}
