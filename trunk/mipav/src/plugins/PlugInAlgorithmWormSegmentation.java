import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.ViewVOIVector;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogGradientMagnitude;
import gov.nih.mipav.view.renderer.WildMagic.Interface.FileSurface_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.SurfaceExtractorCubes;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.BitSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.Vector;

import WildMagic.LibFoundation.Containment.ContBox3f;
import WildMagic.LibFoundation.Distance.DistanceVector3Segment3;
import WildMagic.LibFoundation.Mathematics.Box3f;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Segment3f;
import WildMagic.LibFoundation.Mathematics.Vector2d;
import WildMagic.LibFoundation.Mathematics.Vector3d;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.SceneGraph.BoxBV;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;


public class PlugInAlgorithmWormSegmentation extends PlugInAlgorithmWormStraightening
{
	
	private ModelImage wormImage;
	private double minAngle10 = (Math.PI/18f);
	private double minAngle20 = (Math.PI/9f);
	private double minAngle30 = (Math.PI/6f);
	private double minAngle45 = (Math.PI/4f);
	private double minAngle60 = (Math.PI/3f);
	private double minAngle90 = (Math.PI/2f);

	float min_2P = 0, max_LR = 0, max_head = 0;

	private int midLineMarkerDistance = 55;
    private int midLineClusterDistance = 10;
    
    private int midLineMaxDiameter = 20;
    
    VOIContour head = null;
    VOIContour headEndPoints = null;
    Vector<Vector3f> left_right_markers;
    private float maxPairDistance = -1;
    
    /* The touched, but not relaxed vertices for Dijkstra's search: */
	Vector<Vector3f> border;
    /* Optimization weight: straight-line distance from this vertex to end
     * vertex: */
	float[] remainingWeight;    
    /* Path length along edges from start vertex to this vertex:*/
	float[] weight;
    /* Whether or not this vertex is relaxed: */
	boolean[] relaxed;    
    /* The vertex on the path before this one: */
    int[] previous;
    
    VOIContour headPoints;      
    Vector<VOIContour> headMaskPoints = new Vector<VOIContour>();
    Vector<BitSet> headMasks = new Vector<BitSet>();    
    BitSet headMask = null;    
    BitSet leftRightMask = null;

    private boolean saveLattice = true;
    private boolean saveMesh = false;
    private boolean saveLeftRightMarkers = true;
    private boolean saveIntensityProjection = false;
    
    public PlugInAlgorithmWormSegmentation(ModelImage wormImage)
	{
		this.wormImage = wormImage;
	}
    
    public void runAlgorithm()
	{
    	wormImage.calcMinMax();
    	int dimX = wormImage.getExtents().length > 0 ? wormImage.getExtents()[0] : 1;
    	int dimY = wormImage.getExtents().length > 1 ? wormImage.getExtents()[1] : 1;
    	int dimZ = wormImage.getExtents().length > 2 ? wormImage.getExtents()[2] : 1; 
    	Vector3f min = new Vector3f(dimX, dimY, dimZ);		
    	Vector3f max = new Vector3f(0,0,0);
    	
		Vector<Vector3f> dataPoints = new Vector<Vector3f>();
		// calculate a robust histogram of the data values, used to find the left-right marker thresholds:
		robustHistogram(wormImage, dataPoints);

    	short id = (short) wormImage.getVOIs().getUniqueID();
		VOI axisVOI = new VOI(id, "mainAxis_" + id, VOI.POLYLINE, (float)Math.random() );
		ellipsoidFit( wormImage, axisVOI, dataPoints );
		
//		findLeftRightMarkers(wormImage, (float) wormImage.getMax());
//		return;
		
		// find the left right markers based on the threshold values and clustering...
		findLeftRightMarkers(wormImage, min, max);
		fillMarkers( wormImage, max_LR );
		wormImage.resetVOIs();
		
		// find the mind-line head/pharynx segmentation, used to find the start of the lattice:
		findMidline(wormImage, min, max);
		if ( saveLattice )
		{
			// generate the lattice based on the head segmentation and left-right markers
			initLattice( wormImage, max_LR, 30 );
		}
		if ( saveLeftRightMarkers )
		{
			// save the left-right markers to VOI files:
			saveLeftRightMarkers(wormImage);
		}
		if ( saveIntensityProjection )
		{
			// save the image maximum intensity projection as a .tif file:
			saveMaxIntensityProjection( wormImage, headMask, leftRightMask );
		}
	}
    
    public void setOutputLattice( boolean saveLattice )
    {
    	this.saveLattice = saveLattice;
    }
    
    public void setOutputLeftRight( boolean saveLeftRightMarkers )
    {
    	this.saveLeftRightMarkers = saveLeftRightMarkers;
    }
    
    public void setOutputHead( boolean saveMesh )
    {
    	this.saveMesh = saveMesh;
    }
    
    public void setOutputMaxIntensityProjection( boolean saveIntensityProjection )
    {
    	this.saveIntensityProjection = saveIntensityProjection;
    }
    
    public void smoothThree(TriMesh mesh, int iteration, float lambda, float mu) {

        HashSet[] connections = buildConnections(mesh);

        for ( int k = 0; k < iteration; k++ ) {
           scaleMesh( mesh, lambda, connections );
           scaleMesh( mesh, mu, connections );
        }
    }
        
    
    private void averageContour( VOIContour contour )
    {
    	int distance = midLineClusterDistance / 2;
	
		int headSize = contour.size();    		
		
		int prevSize = headSize;
		int newSize = avgContour(contour, distance);
//		System.err.println( "   " + prevSize + " ==>  " + newSize );
		while ( (newSize < prevSize) && (distance < midLineClusterDistance) )
		{
			prevSize = newSize;
			newSize = avgContour(contour, distance++);
//			System.err.println( "   " + prevSize + " ==>  " + newSize );
		}
		int finalCount = newSize;
		for ( int j = 0; j < finalCount; j++ )
		{
			prevSize = newSize;
			newSize = avgContour(contour, midLineClusterDistance);
//			System.err.println( "   " + prevSize + " ==>  " + newSize );
		} 
		while ( newSize < prevSize )
		{
			prevSize = newSize;
			newSize = avgContour(contour, midLineClusterDistance);
//			System.err.println( "   " + prevSize + " ==>  " + newSize );
		}
//		finalCount = newSize;
//		for ( int j = 0; j < finalCount; j++ )
//		{
//			prevSize = newSize;
//			newSize = avgContour(contour, 3*midLineClusterDistance);
////			System.err.println( "   " + prevSize + " ==>  " + newSize );
//		}    		
    }
    

    private int avgContour( VOIContour head, int distance )
    {
    	VOIContour tempHead = new VOIContour(false);
		for ( int i = 0; i < head.size(); i++ )
		{
			Vector3f pt = head.elementAt(i);
			Vector3f avgPt = new Vector3f(pt);
			int count = 1;
			for ( int j = 0; j < head.size(); j++ )
			{
				if ( i != j )
				{
					if ( pt.distance(head.elementAt(j)) < distance )
					{
						avgPt.add(head.elementAt(j));
						count++;
					}
				}
			}
			if ( count != 0 )
			{
				avgPt.scale(1f/count);
				tempHead.add(avgPt);
			}
		}
		head.clear();
		for ( int i = 0; i < tempHead.size(); i++ )
		{
			if ( !containsNearly(head, tempHead.elementAt(i) ) )
			{
				head.add(tempHead.elementAt(i) );
			}
		}
		
    	for ( int i = head.size() - 1; i >=0; i-- )
    	{
    		for ( int j = i-1; j >=0; j-- )
    		{
    			if ( head.elementAt(i).isEqual( head.elementAt(j) ) )
    			{
    				head.remove(i);
    				break;
    			}
    		}
    	}
		
		return head.size();
    }
    



    
    private int[][] buildCombinations( int n )
    {
//    	int n = 5;
    	int combinations = factorial(n);
//    	System.err.println( n + " " + combinations );
    	int[][] table = new int[combinations][n];

    	for ( int i = 0; i < table.length; i++ )
    	{
    		for ( int j = 0; j < table[i].length; j++ )
    		{
    			table[i][j] = j;
    		}
    	}

    	buildTable( table, 0, 0, n );

//    	int subSet = factorial(n-1);
//    	for ( int i = 0; i < table.length; i++ )
//    	{
//    		if ( (i%subSet) == 0 )
//    		{
//        		System.err.println("");
//    		}
//    		for ( int j = 0; j < table[i].length; j++ )
//    		{
//    			System.err.print( table[i][j] + " " );
//    		}
//    		System.err.println("");
//    	}
    	return table;
    }
    
    /**
     * Builds a list of cross-references, so that connections[i] contains all the vertices that are connected to vertex
     * at i.
     */
    private HashSet[] buildConnections(TriMesh mesh) {
        Iterator iter;
        int index;
        boolean addT1, addT2, addT3;

        int iVQuantity = mesh.VBuffer.GetVertexQuantity();
        HashSet<Integer>[] connections = new HashSet[iVQuantity];

        int iTQuantity = mesh.GetTriangleQuantity();
        for (int i = 0; i < iTQuantity; i++)
        {
            int iV0, iV1, iV2;
            int[] aiTris = new int[3];
            if (!mesh.GetTriangle(i, aiTris) )
            {
                continue;
            }
            iV0 = aiTris[0];            iV1 = aiTris[1];            iV2 = aiTris[2];

            if (connections[iV0] == null) {
                connections[iV0] = new HashSet<Integer>();
            }

            addT2 = true;
            addT3 = true;

            for (iter = connections[iV0].iterator(); iter.hasNext();) {
                index = ((Integer) iter.next()).intValue();

                if (index == iV1) {
                    addT2 = false;
                } else if (index == iV2) {
                    addT3 = false;
                }
            }

            if (addT2) {
                connections[iV0].add(new Integer(iV1));
            }

            if (addT3) {
                connections[iV0].add(new Integer(iV2));
            }

            if (connections[iV1] == null) {
                connections[iV1] = new HashSet<Integer>();
            }

            addT1 = true;
            addT3 = true;

            for (iter = connections[iV1].iterator(); iter.hasNext();) {
                index = ((Integer) iter.next()).intValue();

                if (index == iV0) {
                    addT1 = false;
                } else if (index == iV2) {
                    addT3 = false;
                }
            }

            if (addT1) {
                connections[iV1].add(new Integer(iV0));
            }

            if (addT3) {
                connections[iV1].add(new Integer(iV2));
            }

            if (connections[iV2] == null) {
                connections[iV2] = new HashSet<Integer>();
            }

            addT1 = true;
            addT2 = true;

            for (iter = connections[iV2].iterator(); iter.hasNext();) {
                index = ((Integer) iter.next()).intValue();

                if (index == iV0) {
                    addT1 = false;
                } else if (index == iV1) {
                    addT2 = false;
                }
            }

            if (addT1) {
                connections[iV2].add(new Integer(iV0));
            }

            if (addT2) {
                connections[iV2].add(new Integer(iV1));
            }
        }
        return connections;
    }

    
    private void buildTable( int[][] table, int row, int column, int n )
    {
    	if ( n == 1 )
    		return;
    	int size = factorial(n-1);
    	for ( int i = 0; i < n; i++ )
    	{
    		swapColumn( table, row + i*size, column, column + i, size );
    		buildTable( table, row + i*size, column+1, n-1 );
    	}
    }
    
    private boolean checkDuplicates( VOI lattice1, VOI lattice2 )
    {
    	VOIContour left1 = (VOIContour) lattice1.getCurves().elementAt(0);
    	VOIContour right1 = (VOIContour) lattice1.getCurves().elementAt(1);
    	
    	VOIContour left2 = (VOIContour) lattice2.getCurves().elementAt(0);
    	VOIContour right2 = (VOIContour) lattice2.getCurves().elementAt(1);
    	
    	if ( left1.size() != left2.size() )
    	{
    		return false;
    	}
    	// check that left1==left2 and right1==right2
    	boolean equalVOI = true;
    	for ( int i = 0; i < left1.size(); i++ )
    	{
    		if ( !left1.elementAt(i).equals(left2.elementAt(i)) || !right1.elementAt(i).equals(right2.elementAt(i)) )
    		{
    			equalVOI = false;
    			break;
    		}
    	}
    	if ( equalVOI )
    	{
    		return true;
    	}
    	// check that left1==right2 and right1==left2
    	equalVOI = true;
    	for ( int i = 0; i < left1.size(); i++ )
    	{
    		if ( !left1.elementAt(i).equals(right2.elementAt(i)) || !right1.elementAt(i).equals(left2.elementAt(i)) )
    		{
    			equalVOI = false;
    			break;
    		}
    	}
    	if ( equalVOI )
    	{
    		return true;
    	}
    	
    	// check that they aren't the reverse-order of each other:
    	// check that left1==left2 and right1==right2
    	equalVOI = true;
    	int last = left1.size()-1;
    	for ( int i = 0; i < left1.size(); i++ )
    	{
    		if ( !left1.elementAt(i).equals(left2.elementAt(last-i)) || !right1.elementAt(i).equals(right2.elementAt(last-i)) )
    		{
    			equalVOI = false;
    			break;
    		}
    	}
    	if ( equalVOI )
    	{
    		return true;
    	}
    	equalVOI = true;
    	for ( int i = 0; i < left1.size(); i++ )
    	{
    		if ( !left1.elementAt(i).equals(right2.elementAt(last-i)) || !right1.elementAt(i).equals(left2.elementAt(last-i)) )
    		{
    			equalVOI = false;
    			break;
    		}
    	}
    	if ( equalVOI )
    	{
    		return true;
    	}
    	
    	return false;
    }
    
    
        
    private boolean checkTwists( Vector<Vector3f> left, Vector<Vector3f> right )
    {
    	for ( int i = 0; i < left.size()-1; i++ )
    	{
    		Vector3f edge1a = Vector3f.sub( left.elementAt(i), left.elementAt(i+1) );  edge1a.normalize();
    		Vector3f edge1b = Vector3f.sub( left.elementAt(i+1), right.elementAt(i+1) );  edge1b.normalize();
    		
    		Vector3f edge2a = Vector3f.sub( left.elementAt(i), right.elementAt(i+1) ); edge2a.normalize();
    		Vector3f edge2b = Vector3f.sub( right.elementAt(i+1), right.elementAt(i) ); edge2b.normalize();
    		
    		Vector3f normal1 = Vector3f.cross(edge1a, edge1b); normal1.normalize();
    		Vector3f normal2 = Vector3f.cross(edge2a, edge2b); normal2.normalize();
    		
    		if ( Math.abs(normal1.angle(normal2)) > Math.PI/2f )
    		{
    			return true;
    		}
    	}
    	return false;
    }
    
    /**
	 * Computes a volume mask of the triangle mesh surface. The BitSet mask volume has the same volume dimensions as the current image.
	 * The mask is used to show the surface-plane intersections in the slice views.
	 * @param mesh triangle mesh to convert to a volume mask representation.
	 * @return BitSet mask, which is set to true wherever the triangle mesh intersects the volume voxel.
	 */
	private BitSet computeSurfaceMask( ModelImage image, TriMesh mesh )
	{
    	
    	int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
    	int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
    	int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;   
    	
	    BitSet surfaceMask = null;

		BoxBV kBoundingBox = new BoxBV();
		kBoundingBox.ComputeFromData( mesh.VBuffer );
    	
    	Vector3f[] kBoxCorners = new Vector3f[8];
    	kBoundingBox.GetBox().ComputeVertices( kBoxCorners );
    	Vector3f kMaxBB = new Vector3f( -Float.MAX_VALUE, -Float.MAX_VALUE, -Float.MAX_VALUE );
    	Vector3f kMinBB = new Vector3f(  Float.MAX_VALUE,  Float.MAX_VALUE,  Float.MAX_VALUE );
    	for ( int i = 0; i < kBoxCorners.length; i++ )
    	{
    		kMaxBB.max( kBoxCorners[i] );
    		kMinBB.min( kBoxCorners[i] );
    	}		
			
				
    	surfaceMask = new BitSet( dimX * dimY * dimZ );
		Vector3f min = new Vector3f();
		Vector3f max = new Vector3f();

		int iTQuantity = mesh.GetTriangleQuantity();

		int iV0, iV1, iV2;
		int[] aiTris = new int[3];
		Vector3f kV0 = new Vector3f();
		Vector3f kV1 = new Vector3f();
		Vector3f kV2 = new Vector3f();

		for (int i = 0; i < iTQuantity; i++)
		{
			if (!mesh.GetTriangle(i,aiTris) )
			{
				continue;
			}

			iV0 = aiTris[0];
			iV1 = aiTris[1];
			iV2 = aiTris[2];

			mesh.VBuffer.GetPosition3(iV0, kV0);
			mesh.VBuffer.GetPosition3(iV1, kV1);
			mesh.VBuffer.GetPosition3(iV2, kV2);

			// compute the axis-aligned bounding box of the triangle
			min.copy( kV0 );
			min.min( kV1 );
			min.min( kV2 );
			
			max.copy( kV0 );
			max.max( kV1 );
			max.max( kV2 );
			// Rasterize the triangle.  The rasterization is repeated in all
			// three coordinate directions to make sure that floating point
			// round-off errors do not cause any holes in the rasterized
			// surface.
			float iXMin = min.X, iXMax = max.X;
			float iYMin = min.Y, iYMax = max.Y;
			float iZMin = min.Z, iZMax = max.Z;
			int ptr;
			int end = surfaceMask.size();

			for (float iY = iYMin; iY < iYMax; iY = iY + 0.1f) {

				for (float iZ = iZMin; iZ < iZMax; iZ = iZ + 0.1f) {
					float iX = getIntersectX(kV0, kV1, kV2, iY, iZ);

					if (iX != -1) {
						ptr = Math.round(iX) + (dimX * (Math.round(iY) + (dimY * Math.round(iZ))));
						if ( (ptr >= 0) && (ptr < end)) {
							surfaceMask.set(ptr);
						}
					}
				}
			}

			for (float iX = iXMin; iX < iXMax; iX = iX + 0.1f) {

				for (float iZ = iZMin; iZ < iZMax; iZ = iZ + 0.1f) {
					float iY = getIntersectY(kV0, kV1, kV2, iX, iZ);

					if (iY != -1) {
						ptr = Math.round(iX) + (dimX * (Math.round(iY) + (dimY * Math.round(iZ))));
						if ( (ptr >= 0) && (ptr < end)) {
							surfaceMask.set(ptr);
						}
					}
				}
			}

			for (float iX = iXMin; iX < iXMax; iX = iX + 0.1f) {

				for (float iY = iYMin; iY < iYMax; iY = iY + 0.1f) {
					float iZ = getIntersectZ(kV0, kV1, kV2, iX, iY);

					if (iZ != -1) {
						ptr = Math.round(iX) + (dimX * (Math.round(iY) + (dimY * Math.round(iZ))));
						if ( (ptr >= 0) && (ptr < end)) {
							surfaceMask.set(ptr);
						}
					}
				}
            }
        }		
		return surfaceMask;
	}
    
    private boolean containsNearly( VOIContour contour, Vector3f pt )
    {
    	for ( int i = 0; i < contour.size(); i++ )
    	{
    		if ( contour.elementAt(i).isEqual(pt) )
    		{
    			return true;
    		}
    	}
    	return false;
    }
    
    private Vector2d costFunction( Vector<Vector3f> left, Vector<Vector3f> right, boolean useIncreasingOrder, boolean print  )
    {
    	Vector2d cost = new Vector2d();
    	if ( checkTwists(left, right) )
    	{
        	cost.X = Float.MAX_VALUE;    		
        	return cost;    		
    	}
    	int size = Math.min ( left.size(), right.size() );
    	// average and variance of successive pair angles:
    	double meanAngle = 0;
    	boolean increasingLengths = true;
    	for ( int i = 0; i < size-1; i++ )
    	{
    		Vector3f pair0 = Vector3f.sub( right.elementAt(i), left.elementAt(i) );
    		float length = pair0.normalize();
    		
    		Vector3f pair1 = Vector3f.sub( right.elementAt(i+1), left.elementAt(i+1) );
    		float lengthP1 = pair1.normalize();
    		if ( (length - midLineClusterDistance) > lengthP1 )
    		{
    			increasingLengths = false;
    		}
    		
    		float angle = Math.abs(pair1.angle(pair0));
//    		if ( angle > Math.PI/1.5f )
    		if ( angle > Math.PI/2f )
    		{
            	cost.X = Float.MAX_VALUE;    		
            	return cost;
    		}
    		meanAngle += angle;
    	}
    	float pairsParallel = (float) meanAngle;    	

    	meanAngle = 0;
    	for ( int i = 0; i < size-1; i++ )
    	{
    		Vector3f rightEdge = Vector3f.sub( right.elementAt(i+1), right.elementAt(i) );
    		rightEdge.normalize();
    		
    		Vector3f leftEdge = Vector3f.sub( left.elementAt(i+1), left.elementAt(i) );
    		leftEdge.normalize();
    		
    		float angle = Math.abs(leftEdge.angle(rightEdge));
    		if ( angle > Math.PI/6f )
    		{
            	cost.X = Float.MAX_VALUE;    		
            	return cost;
    		}
    		meanAngle += angle;
    	}
    	float sidesParallel = (float) meanAngle;
    	
    	

//    	Vector3f posEnd = null;
//    	Vector3f negEnd = null;
//    	float halfLength = 1;
//    	if ( (majorAxis != null) && (majorAxis.getCurves().size() >= 1) )
//    	{
//    		posEnd = majorAxis.getCurves().elementAt(0).elementAt(0);
//    		negEnd = majorAxis.getCurves().elementAt(0).elementAt(1);
//    		halfLength = posEnd.distance(negEnd)/2.0f;
//    	}
    	
    	double meanAngleL = 0;
    	double meanAngleR = 0;
    	for ( int i = 1; i < size-1; i++ )
    	{
    		Vector3f rightEdge0 = Vector3f.sub( right.elementAt(i), right.elementAt(i-1) );
    		rightEdge0.normalize();
    		Vector3f rightEdge1 = Vector3f.sub( right.elementAt(i+1), right.elementAt(i) );
    		rightEdge1.normalize();    		
    		float angleR = rightEdge0.angle(rightEdge1);
//    		if ( posEnd != null && negEnd != null )
//    		{
//    			float distP = right.elementAt(i).distance(posEnd);
//    			float distE = right.elementAt(i).distance(posEnd);
//    			if ( distP < distE )
//    			{
//    				angleR *= (distP/halfLength);
//    			}
//    			else
//    			{
//    				angleR *= (distE/halfLength);
//    			}
//    		}
    		meanAngleR += angleR;
    		
    		Vector3f leftEdge0 = Vector3f.sub( left.elementAt(i), left.elementAt(i-1) );
    		leftEdge0.normalize();    		
    		Vector3f leftEdge1 = Vector3f.sub( left.elementAt(i+1), left.elementAt(i) );
    		leftEdge1.normalize();    		
    		float angleL = leftEdge0.angle(leftEdge1);
//    		if ( posEnd != null && negEnd != null )
//    		{
//    			float distP = left.elementAt(i).distance(posEnd);
//    			float distE = left.elementAt(i).distance(posEnd);
//    			if ( distP < distE )
//    			{
//    				angleL *= (distP/halfLength);
//    			}
//    			else
//    			{
//    				angleL *= (distE/halfLength);
//    			}
//    		}
    		meanAngleL += angleL;
    	}
    	float curvature = (float) (meanAngleL + meanAngleR);

    	float lengthL = 0;
    	float lengthR = 0;
    	float totalLength = 0;
    	for ( int i = 0; i < size-1; i++ )
    	{
    		lengthL += left.elementAt(i).distance( left.elementAt(i+1) );
    		lengthR += right.elementAt(i).distance( right.elementAt(i+1) );
    	}
    	totalLength = lengthL + lengthR;
    	float lengthSame = 10*Math.abs(lengthL - lengthR)/totalLength;
    	float increasingLengthsCost = increasingLengths ? 0 : 1;
    	if ( !useIncreasingOrder )
    	{
    		increasingLengthsCost = 0;
    	}
    	
    	sidesParallel *= 1.2;
    	if ( print )
    	{
    		System.err.println( "   size = " + size + " " + Math.max(0, 3-size) );
    		System.err.println( "   curvature = " + curvature  + " " + (curvature / (size)));
    		System.err.println( "   sidesParallel = " + sidesParallel  + " " +  (sidesParallel / (2*size)));
    		System.err.println( "   pairsParallel = " + pairsParallel  + " " +  (pairsParallel / (2*size)));
    		System.err.println( "   lengthSame = " + lengthSame );
    		System.err.println( "   totalLength = " + (totalLength/size) );
    		System.err.println( "   increasingLengthsCost = " + increasingLengthsCost );    		
    	}
    	
    	int sizeCost = Math.max( 0, 3-size );
    	if ( !useIncreasingOrder )
    	{
    		sizeCost = Math.max( 0, (5-size) );
    	}
    	cost.X = sizeCost  + (curvature / (size)) + (sidesParallel / (size)) + (pairsParallel / (size))  + 
    			lengthSame + increasingLengthsCost;
//    	cost.Set(0, Math.abs(1f/(float)left.size()));
//    	cost.Set(1, curvature);
//    	cost.Set(2, increasingLengthsCost);
//    	cost.Set(3, sidesParallel);
////    	cost.Set(3, pairsParallel);
//    	cost.Set(4, lengthSame);
    	return cost;
    }

    private Vector2d costFunctionGrow( Vector<Vector3f> left, Vector<Vector3f> right  )
    {
    	Vector2d cost = new Vector2d();
    	if ( checkTwists(left, right) )
    	{
        	cost.X = Float.MAX_VALUE;    		
        	return cost;    		
    	}
    	int size = Math.min ( left.size(), right.size() );
    	// average and variance of successive pair angles:
    	double meanAngle = 0;
    	boolean increasingLengths = true;
    	for ( int i = 0; i < size-1; i++ )
    	{
    		Vector3f pair0 = Vector3f.sub( right.elementAt(i), left.elementAt(i) );
    		float length = pair0.normalize();
    		
    		Vector3f pair1 = Vector3f.sub( right.elementAt(i+1), left.elementAt(i+1) );
    		float lengthP1 = pair1.normalize();
    		if ( (length - midLineClusterDistance) > lengthP1 )
    		{
    			increasingLengths = false;
    		}
    		
    		float angle = Math.abs(pair1.angle(pair0));
//    		if ( angle > Math.PI/1.5f )
    		if ( angle > Math.PI/2f )
    		{
            	cost.X = Float.MAX_VALUE;    		
            	return cost;
    		}
    		meanAngle += angle;
    	}
    	float pairsParallel = (float) meanAngle;    	

    	meanAngle = 0;
    	for ( int i = 0; i < size-1; i++ )
    	{
    		Vector3f rightEdge = Vector3f.sub( right.elementAt(i+1), right.elementAt(i) );
    		rightEdge.normalize();
    		
    		Vector3f leftEdge = Vector3f.sub( left.elementAt(i+1), left.elementAt(i) );
    		leftEdge.normalize();
    		
    		float angle = Math.abs(leftEdge.angle(rightEdge));
    		if ( angle > Math.PI/6f )
    		{
            	cost.X = Float.MAX_VALUE;    		
            	return cost;
    		}
    		meanAngle += angle;
    	}
    	float sidesParallel = (float) meanAngle;
    	
    	

//    	Vector3f posEnd = null;
//    	Vector3f negEnd = null;
//    	float halfLength = 1;
//    	if ( (majorAxis != null) && (majorAxis.getCurves().size() >= 1) )
//    	{
//    		posEnd = majorAxis.getCurves().elementAt(0).elementAt(0);
//    		negEnd = majorAxis.getCurves().elementAt(0).elementAt(1);
//    		halfLength = posEnd.distance(negEnd)/2.0f;
//    	}
    	
    	double meanAngleL = 0;
    	double meanAngleR = 0;
    	for ( int i = 1; i < size-1; i++ )
    	{
    		Vector3f rightEdge0 = Vector3f.sub( right.elementAt(i), right.elementAt(i-1) );
    		rightEdge0.normalize();
    		Vector3f rightEdge1 = Vector3f.sub( right.elementAt(i+1), right.elementAt(i) );
    		rightEdge1.normalize();    		
    		float angleR = rightEdge0.angle(rightEdge1);
//    		if ( posEnd != null && negEnd != null )
//    		{
//    			float distP = right.elementAt(i).distance(posEnd);
//    			float distE = right.elementAt(i).distance(posEnd);
//    			if ( distP < distE )
//    			{
//    				angleR *= (distP/halfLength);
//    			}
//    			else
//    			{
//    				angleR *= (distE/halfLength);
//    			}
//    		}
    		meanAngleR += angleR;
    		
    		Vector3f leftEdge0 = Vector3f.sub( left.elementAt(i), left.elementAt(i-1) );
    		leftEdge0.normalize();    		
    		Vector3f leftEdge1 = Vector3f.sub( left.elementAt(i+1), left.elementAt(i) );
    		leftEdge1.normalize();    		
    		float angleL = leftEdge0.angle(leftEdge1);
//    		if ( posEnd != null && negEnd != null )
//    		{
//    			float distP = left.elementAt(i).distance(posEnd);
//    			float distE = left.elementAt(i).distance(posEnd);
//    			if ( distP < distE )
//    			{
//    				angleL *= (distP/halfLength);
//    			}
//    			else
//    			{
//    				angleL *= (distE/halfLength);
//    			}
//    		}
    		meanAngleL += angleL;
    	}
    	float curvature = (float) (meanAngleL + meanAngleR);

    	float lengthL = 0;
    	float lengthR = 0;
    	float totalLength = 0;
    	for ( int i = 0; i < size-1; i++ )
    	{
    		lengthL += left.elementAt(i).distance( left.elementAt(i+1) );
    		lengthR += right.elementAt(i).distance( right.elementAt(i+1) );
    	}
    	totalLength = lengthL + lengthR;
    	float lengthSame = 10*Math.abs(lengthL - lengthR)/totalLength;
    	
    	sidesParallel *= 1.2;
    	
    	int sizeCost = Math.max( 0, (5-size) );
    	
    	cost.X = sizeCost  + (curvature / (size)) + (sidesParallel / (size)) + (pairsParallel / (size))  + 
    			lengthSame;
    	return cost;
    }
    
    private BitSet createMask( ModelImage image, TriMesh mesh, Vector3f pt, boolean show, String postScript )
	{
		BitSet meshMask = computeSurfaceMask(image, mesh);	
		floodFill( image, meshMask, pt);
		if ( show )
		{
			ModelImage maskImage = (ModelImage)srcImage.clone();
			maskImage.setMask(meshMask);
			maskImage.setImageName( srcImage.getImageName() + postScript );        	
			new ViewJFrameImage(maskImage);		
		}
		return meshMask;
	}
    
    
    private TriMesh createMesh( ModelImage image, BitSet mask )
    {
    	int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
    	int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
    	int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
    	
    	int length = dimX * dimY * dimZ;
    	int[] buffer = new int[length];
    	for ( int z = 0; z < dimZ; z++ )
    	{
    		for ( int y = 0; y < dimY; y++ )
    		{
    			for ( int x = 0; x < dimX; x++ )
    			{
    				int index = z*dimY*dimX + y*dimX + x;
    				if ( !mask.get(index) )
    				{
    					buffer[index] = 0;
    				}
    				else
    				{
    					buffer[index] = 1;
    				}
    				if ( (z == 0) || (z == dimZ -1) )
    				{
    					buffer[index] = 0;
    				}						
    			}
    		}
    	}

    	final SurfaceExtractorCubes kExtractor = new SurfaceExtractorCubes(dimX, dimY, dimZ, buffer,
    			1, 1, 1, null, null, null);
    	final TriMesh kMesh = kExtractor.getLevelSurface(0);
    	return kMesh;

    }
    
    private float createPath( ModelImage image, int start, int end, Vector<Vector3f>path )
    {
        /* If the start == end, return 0 - no path: */
        if (start == end) {
            return 0;
        }
    	int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
    	int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
    	int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;    
        
    	int temp = end;
        int x = temp % dimX;
        temp -= x;
        temp /= dimX;
        int y = temp % dimY;
        temp -= y;
        temp /= dimY;
        int z = temp;
        
        path.add( new Vector3f(x,y,z) );

        int node = end;
        int prev = previous[node];

        if (prev == -1) {
            return 0;
        }

        while (prev != start) {
        	temp = prev;
            x = temp % dimX;
            temp -= x;
            temp /= dimX;
            y = temp % dimY;
            temp -= y;
            temp /= dimY;
            z = temp;
            path.add( 0, new Vector3f(x,y,z) );
            /* Move to the next point: */
        	node = prev;
            prev = previous[node];
        }
        
        float pathLength = 0;
        for ( int i = 0; i < path.size() - 1; i++ )
        {
        	pathLength += path.elementAt(i).distance(path.elementAt(i+1) );
        }
        return pathLength;
    }
    
    
    private Vector3f ellipsoidFit( ModelImage image, VOI axisVOI, Vector<Vector3f> dataPoints )
    {	    	
    	if ( dataPoints == null )
    	{
    		return null;
    	}
//		System.err.println( "ellipsoidFit " + dataPoints.size() );
    	if ( dataPoints.size() == 0 )
    	{
    		return null;
    	}
    	int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
    	int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
    	int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;   
    	
//    	AlgorithmEllipsoidFit fitData = new AlgorithmEllipsoidFit(dataPoints);
//    	double[] axes = fitData.getAxes();
//    	System.err.println( axes[0] + " " + axes[1] + " " + axes[2] );
    	
        Box3f kBox = ContBox3f.ContOrientedBox(dataPoints.size(), dataPoints);
		
		int maxI = 2;
		float maxV = -Float.MAX_VALUE;
    	for ( int i = 0; i < kBox.Extent.length; i++ )
    	{
    		if ( kBox.Extent[i] > maxV )
    		{
    			maxV = kBox.Extent[i];
    			maxI = i;
    		}
    	}
//    	System.err.println( "ellipsoid fit " + kBox.Extent[0] + " " + kBox.Extent[1] + " " + kBox.Extent[2] );
    	
        kBox.Extent[0] += 10;
        kBox.Extent[1] += 10;
        kBox.Extent[2] += 10;
        
		Vector3f posEnd = new Vector3f(kBox.Axis[maxI]);  posEnd.scale(kBox.Extent[maxI] );
		posEnd.add(kBox.Center);
		Vector3f negEnd = new Vector3f(kBox.Axis[maxI]); negEnd.scale(-kBox.Extent[maxI] );
		negEnd.add(kBox.Center);
		
		while ( posEnd.X >= dimX || posEnd.Y >= dimY || posEnd.Z >= dimZ )
		{
//			System.err.println( posEnd );
			posEnd.sub(kBox.Center);
			posEnd.scale( 0.95f );
			posEnd.add(kBox.Center);
		}
		while ( negEnd.X >= dimX || negEnd.Y >= dimY || negEnd.Z >= dimZ )
		{
//			System.err.println( negEnd );
			negEnd.sub(kBox.Center);
			negEnd.scale( 0.95f );
			negEnd.add(kBox.Center);
		}
		
		

		VOIContour mainAxis = new VOIContour(false);
		mainAxis.add( posEnd );
		mainAxis.add( negEnd );
		axisVOI.getCurves().add(mainAxis);
		axisVOI.setColor( new Color( 255, 255, 0) );
		mainAxis.update( new ColorRGBA(1,1,0,1));
//		image.registerVOI( axisVOI );

//		System.err.println( posEnd );
//		System.err.println( negEnd );
    	
		Vector3f axis = Vector3f.sub(posEnd, negEnd);
		axis.normalize();
		return axis;
    		
//		 The contour is 3-dimensional (not entirely on one of the three orthogonal planes):
//		Matrix3f kMat = new Matrix3f();
//		float[] afScale = new float[3];
//		Vector3f rkU = new Vector3f(0,-1,0);
//		new ApprEllipsoidFit3f( dataPoints.size(), dataPoints, rkU, kMat, afScale );
//		Vector3f kX = new Vector3f( kMat.M00, kMat.M10, kMat.M20 );  kX.normalize();
//		Vector3f kY = new Vector3f( kMat.M01, kMat.M11, kMat.M21 );  kY.normalize();
//		Vector3f kZ = new Vector3f( kMat.M02, kMat.M12, kMat.M22 );  kZ.normalize();	
//		Ellipsoid3f ellipsoid = new Ellipsoid3f( rkU, kX, kY, kZ, afScale[0], afScale[1], afScale[2] );
		
		
		
		
		
		
//		Vector3f rkU = kBox.Center;
//		Vector3f kX = kBox.Axis[0];
//		Vector3f kY = kBox.Axis[1];
//		Vector3f kZ = kBox.Axis[2];
//		float[] afScale = new float[]{kBox.Extent[0], kBox.Extent[1], kBox.Extent[2]};
//		Ellipsoid3f ellipsoid = new Ellipsoid3f( kBox.Center, kBox.Axis[0], kBox.Axis[1], kBox.Axis[2], kBox.Extent[0],  kBox.Extent[1],  kBox.Extent[2] );
   	

//		System.err.println( rkU );
//		System.err.println( kX + "     " + afScale[0] );
//		System.err.println( kY + "     " + afScale[1] );
//		System.err.println( kZ + "     " + afScale[2] );
		
		
//		BitSet mask = new BitSet(dimX*dimY*dimZ);
//		Vector3f test = new Vector3f();
//		for ( int z = 0; z < dimZ; z++ )
//		{
//			for ( int y = 0; y < dimY; y++ )
//			{
//				for ( int x = 0; x < dimX; x++ )
//				{
//					test.set(x,y,z);
//					if ( ellipsoid.Contains( test ) )
//					{
//						mask.set( z * dimX*dimY + y * dimX + x );
//					}
//				}
//			}
//		}
//		image.setMask(mask);
				
//        System.err.println( "ellipsoidFit " + mask.cardinality() );
//        System.err.println( "ellipsoidFit " + dimX * dimY * dimZ );
    }
    
    private int factorial( int n )
    {
    	int combinations = 1;
    	for ( int i = 1; i <= n; i++ )
    	{
    		combinations *= i;
    	}
    	return combinations;
    }
    
    private void fill( ModelImage image, float intensityMin, Vector<Vector3f> seedList, BitSet visited, BitSet mask, Vector<Vector3f> maskPoints )
    {
    	int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
    	int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
    	int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
    	
//    	int radiusMax = 110;
//    	int radiusMin = 90;
//    	Vector3f min = new Vector3f(dimX, dimY, dimZ);
//    	Vector3f max = new Vector3f(0,0,0);
//		getBoundsDistance( min, max, centerBound, radiusMax, radiusMin );
    	while ( seedList.size() > 0 )
    	{
    		Vector3f seed = seedList.remove(0);

    		boolean edgeFound = false;
    		int step = 1;
    		for ( int z = (int) (seed.Z-step); z <= seed.Z+step; z++ )
    		{
    			for ( int y = (int) (seed.Y-step); y <= seed.Y+step; y++ )
    			{
    				for ( int x = (int) (seed.X-step); x <= seed.X+step; x++ )
    				{
    					if ( (x >= 0) && (x < dimX) && (y >= 0) && (y < dimY) && (z >= 0) && (z < dimZ) )
    					{
    						if ( x != seed.X || y != seed.Y || z != seed.Z )
    						{
								float value = image.getFloat(x, y, z);
								if ( value < intensityMin )
								{
									edgeFound = true;
								}
    						}
    					}
    				}
    			}
    		}

			for ( int z = (int) (seed.Z-1); z <= seed.Z+1; z++ )
			{
				for ( int y = (int) (seed.Y-1); y <= seed.Y+1; y++ )
				{
					for ( int x = (int) (seed.X-1); x <= seed.X+1; x++ )
					{
						if ( (x >= 0) && (x < dimX) && (y >= 0) && (y < dimY) && (z >= 0) && (z < dimZ) )
						{
							if ( x != seed.X || y != seed.Y || z != seed.Z )
							{
								int index = (z * dimX * dimY + y * dimX + x);

								if ( !visited.get(index) )
								{
									visited.set(index);
//									if ( (x >= min.X) && (x <= max.X) && (y >= min.Y) && (y <= max.Y) && (z >= min.Z) && (z <= max.Z) )
									{
										if ( !edgeFound )
										{
//											if ( distance( centerBound, x, y, z ) < radiusMax )
											{
												float value = image.getFloat(x, y, z);
												if ( value >= intensityMin )
												{
													mask.set(index);
													maskPoints.add( new Vector3f(x, y, z) );
													seedList.add( new Vector3f(x, y, z) );
												}
											}
										}
									}
    							}
    						}
    					}
    				}
    			}
    		}
    	}
    }
    
    private void fill( ModelImage image, Vector<Vector3f> seedList, BitSet visited, BitSet mask )
    {    	
    	
    	int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
    	int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
    	int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;    
    	
    	while ( seedList.size() > 0 )
    	{
    		Vector3f seed = seedList.remove(0);

    		int x = (int) seed.X;
    		int y = (int) seed.Y;
    		int z = (int) (seed.Z -1);
    		if ( (x >= 0) && (x < dimX) && (y >= 0) && (y < dimY) && (z >= 0) && (z < dimZ) )
    		{
    			int index = (z * dimX * dimY + y * dimX + x);
    			if ( !visited.get(index) )
    			{
    				visited.set(index);
    				if ( !mask.get(index) )
    				{
    					mask.set(index);
    					seedList.add( new Vector3f(x, y, z) );
    				}
    			}
    		}

    		x = (int) seed.X;
    		y = (int) seed.Y;
    		z = (int) (seed.Z +1);
    		if ( (x >= 0) && (x < dimX) && (y >= 0) && (y < dimY) && (z >= 0) && (z < dimZ) )
    		{
    			int index = (z * dimX * dimY + y * dimX + x);
    			if ( !visited.get(index) )
    			{
    				visited.set(index);
    				if ( !mask.get(index) )
    				{
    					mask.set(index);
    					seedList.add( new Vector3f(x, y, z) );
    				}
    			}
    		}

    		x = (int) seed.X;
    		y = (int) (seed.Y - 1);
    		z = (int) seed.Z;
    		if ( (x >= 0) && (x < dimX) && (y >= 0) && (y < dimY) && (z >= 0) && (z < dimZ) )
    		{
    			int index = (z * dimX * dimY + y * dimX + x);
    			if ( !visited.get(index) )
    			{
    				visited.set(index);
    				if ( !mask.get(index) )
    				{
    					mask.set(index);
    					seedList.add( new Vector3f(x, y, z) );
    				}
    			}
    		}

    		x = (int) seed.X;
    		y = (int) (seed.Y + 1);
    		z = (int) seed.Z;
    		if ( (x >= 0) && (x < dimX) && (y >= 0) && (y < dimY) && (z >= 0) && (z < dimZ) )
    		{
    			int index = (z * dimX * dimY + y * dimX + x);
    			if ( !visited.get(index) )
    			{
    				visited.set(index);
    				if ( !mask.get(index) )
    				{
    					mask.set(index);
    					seedList.add( new Vector3f(x, y, z) );
    				}
    			}
    		}

    		x = (int) (seed.X - 1);
    		y = (int) seed.Y;
    		z = (int) seed.Z;
    		if ( (x >= 0) && (x < dimX) && (y >= 0) && (y < dimY) && (z >= 0) && (z < dimZ) )
    		{
    			int index = (z * dimX * dimY + y * dimX + x);
    			if ( !visited.get(index) )
    			{
    				visited.set(index);
    				if ( !mask.get(index) )
    				{
    					mask.set(index);
    					seedList.add( new Vector3f(x, y, z) );
    				}
    			}
    		}

    		x = (int) (seed.X + 1);
    		y = (int) seed.Y;
    		z = (int) seed.Z;
    		if ( (x >= 0) && (x < dimX) && (y >= 0) && (y < dimY) && (z >= 0) && (z < dimZ) )
    		{
    			int index = (z * dimX * dimY + y * dimX + x);
    			if ( !visited.get(index) )
    			{
    				visited.set(index);
    				if ( !mask.get(index) )
    				{
    					mask.set(index);
    					seedList.add( new Vector3f(x, y, z) );
    				}
    			}
    		}
    	}
    }
    
    
    private void fillHead( ModelImage image, float intensityMin )
    {    	
    	if ( head == null )
    	{
    		return;
    	}
    	for ( int i = 0; i < head.size(); i++ )
    	{
    		head.elementAt(i).X = Math.round(head.elementAt(i).X);
    		head.elementAt(i).Y = Math.round(head.elementAt(i).Y);
    		head.elementAt(i).Z = Math.round(head.elementAt(i).Z);
    	}
		for ( int i = head.size() - 1; i >=0; i-- )
		{
			for ( int j = i-1; j >=0; j-- )
			{
				if ( head.elementAt(i).isEqual( head.elementAt(j) ) )
				{
					head.remove(i);
					break;
				}
			}
		}
    	
    	
//		System.err.println( "Left Right mask " + leftRightMask.cardinality() );
//		System.err.println( "Head mask " + headMask.cardinality() );
    	
    	int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
    	int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
    	int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;    
    	int length = dimX * dimY * dimZ;
    	if ( headMask == null )
    	{
        	headMask = new BitSet(length);
    	}
    	int count = 0;
    	while ( head.size() > 0 )
    	{
    		BitSet visited = new BitSet(length);
    		BitSet mask = new BitSet(length);
    		VOIContour maskPoints = new VOIContour(false);

    		Vector<Vector3f> seedList = new Vector<Vector3f>();
    		Vector3f seed = head.lastElement();
    		head.remove(seed);
    		int index = (int) (seed.Z * dimX * dimY + seed.Y * dimX + seed.X);	
    		visited.set(index);
    		maskPoints.add(seed);
    		seedList.add(seed);
    		
        	fillHead( image, intensityMin, seedList, visited, mask, maskPoints );
//    		System.err.print( count++ + " " + head.size() + " " + maskPoints.size() );
        	for ( int i = 0; i < maskPoints.size(); i++ )
        	{
        		if ( head.contains(maskPoints.elementAt(i) ) )
        		{
        			head.remove(maskPoints.elementAt(i));
        		}
        	}
//        	System.err.println( "    " + head.size() );
        	if ( maskPoints.size() > 0 )
        	{
            	Box3f kBox = ContBox3f.ContOrientedBox(maskPoints.size(), maskPoints);
        		boolean headFound = false;
        		boolean lr_found = true;
        		if ( (kBox.Extent[0] >= 5) && (kBox.Extent[1] >= 5) && (kBox.Extent[2] >= 5) )
        		{
//        			System.err.println( kBox.Center + "     " + kBox.Extent[0] + " " + kBox.Extent[1] + " " + kBox.Extent[2] );

        			for ( int j = 0; j < kBox.Extent.length; j++ )
        			{
        				if ( (kBox.Extent[j] > 30) )
        				{
        					headFound = true;
        					lr_found = false;
        					break;
        				}
        			}
        			if (lr_found)
        			{
//        				leftRightMask.or(mask);
//        				left_right_markers.add(new Vector3f(seed) );
        			}
        			if (headFound)
        			{
        				headMasks.add(mask);
        				headMaskPoints.add(maskPoints);
        				headMask.or(mask);
        			}
        		}
        	}
    	}    	


//		System.err.println( "Left Right mask " + leftRightMask.cardinality() );
//		image.setMask( leftRightMask );
//		ModelImage newImage = (ModelImage)image.clone();
//		newImage.setImageName( JDialogBase.makeImageName( image.getImageName(), "_lr_mask.xml" ) );
//		new ViewJFrameImage(newImage);


//		System.err.println( "Head mask " + headMask.cardinality() );
//		image.setMask( headMask );
//		newImage = (ModelImage)image.clone();
//		newImage.setImageName( JDialogBase.makeImageName( image.getImageName(), "_head_mask.xml" ) );
//		new ViewJFrameImage(newImage);
		
		headMask.clear();
//		System.err.println( headMasks.size() );
		for ( int i = headMasks.size() - 1; i >=0; i-- )
		{
			for ( int j = i-1; j >= 0; j-- )
			{
				if ( headMasks.elementAt(i).intersects( headMasks.elementAt(j) ) )
				{
					headMasks.elementAt(j).or(headMasks.elementAt(i) );
					headMasks.remove(i);
					
					headMaskPoints.elementAt(j).addAll(headMaskPoints.elementAt(i) );
					headMaskPoints.remove(i);
					break;
				}
			}
		}
//		System.err.println( headMasks.size() );
		headPoints = new VOIContour(false);
		for ( int i = 0; i < headMasks.size(); i++ )
		{
			headMask.or(headMasks.elementAt(i));
			headPoints.addAll(headMaskPoints.elementAt(i));
			if ( saveMesh )
			{
				TriMesh mesh = createMesh( image, headMasks.elementAt(i) );
				smoothThree(mesh, 50, 0.33f, -0.34f);
				saveMesh( image, mesh, true, i );
			}
		}
    }


    private void fillHead( ModelImage image, float intensityMin, Vector<Vector3f> seedList, BitSet visited, BitSet mask, Vector<Vector3f> maskPoints )
    {
    	int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
    	int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
    	int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
    	
//    	int radiusMax = 110;
//    	int radiusMin = 90;
//    	Vector3f min = new Vector3f(dimX, dimY, dimZ);
//    	Vector3f max = new Vector3f(0,0,0);
//		getBoundsDistance( min, max, centerBound, radiusMax, radiusMin );
    	while ( seedList.size() > 0 )
    	{
    		Vector3f seed = seedList.remove(0);

    		boolean edgeFound = false;
    		int step = 1;
    		for ( int z = (int) (seed.Z-step); z <= seed.Z+step; z++ )
    		{
    			for ( int y = (int) (seed.Y-step); y <= seed.Y+step; y++ )
    			{
    				for ( int x = (int) (seed.X-step); x <= seed.X+step; x++ )
    				{
    					if ( (x >= 0) && (x < dimX) && (y >= 0) && (y < dimY) && (z >= 0) && (z < dimZ) )
    					{
    						if ( x != seed.X || y != seed.Y || z != seed.Z )
    						{
								float value = image.getFloat(x, y, z);
								if ( value < intensityMin )
								{
									edgeFound = true;
								}
    						}
    					}
    				}
    			}
    		}

			for ( int z = (int) (seed.Z-1); z <= seed.Z+1; z++ )
			{
				for ( int y = (int) (seed.Y-1); y <= seed.Y+1; y++ )
				{
					for ( int x = (int) (seed.X-1); x <= seed.X+1; x++ )
					{
						if ( (x >= 0) && (x < dimX) && (y >= 0) && (y < dimY) && (z >= 0) && (z < dimZ) )
						{
							if ( x != seed.X || y != seed.Y || z != seed.Z )
							{
								int index = (z * dimX * dimY + y * dimX + x);
								if ( !visited.get(index) )
								{
									visited.set(index);
//									if ( (x >= min.X) && (x <= max.X) && (y >= min.Y) && (y <= max.Y) && (z >= min.Z) && (z <= max.Z) )
									{
										if ( !edgeFound )
										{
//											if ( distance( centerBound, x, y, z ) < radiusMax )
											{
												float value = image.getFloat(x, y, z);
												if ( value >= intensityMin )
												{
													mask.set(index);
													maskPoints.add( new Vector3f(x, y, z) );
													seedList.add( new Vector3f(x, y, z) );
												}
											}

											if ( leftRightMask.get(index) )
											{
												mask.clear();
												maskPoints.clear();
												return;
											}
										}
									}
    							}
    						}
    					}
    				}
    			}
    		}
    	}
    }

    private void fillMarkers( ModelImage image, float intensityMin )
    {
    	int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
    	int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
    	int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;    
    	
    	if ( left_right_markers == null )
    	{
    		return;
    	}
    	int length = dimX * dimY * dimZ;
    	headMask = new BitSet(length);
    	leftRightMask = new BitSet(length);
//    	System.err.println( "   fillMarkers " + left_right_markers.size() );
    	for ( int i = left_right_markers.size() - 1; i >=0; i-- )
    	{
    		BitSet visited = new BitSet(length);
    		BitSet mask = new BitSet(length);
    		VOIContour maskPoints = new VOIContour(false);

    		Vector<Vector3f> seedList = new Vector<Vector3f>();
    		Vector3f seed = left_right_markers.elementAt(i);    		
    		int index = (int) (seed.Z * dimX * dimY + seed.Y * dimX + seed.X);	
    		visited.set(index);
    		maskPoints.add(seed);
    		seedList.add(seed);
    		
        	fill( image, intensityMin, seedList, visited, mask, maskPoints );
            Box3f kBox = ContBox3f.ContOrientedBox(maskPoints.size(), maskPoints);
//            System.err.println( "Seed " + i + " " + kBox.Extent[0] + " " + kBox.Extent[1] + " " + kBox.Extent[2] );
            boolean lr_found = true;
            boolean head_found = false;
            for ( int j = 0; j < kBox.Extent.length; j++ )
            {
            	if ( (kBox.Extent[j] > 30) )
            	{
            		left_right_markers.remove(i);
            		lr_found = false;
            		head_found = true;
            		break;
            	}
            }
        	if ( lr_found && (kBox.Extent[0] < 5) && (kBox.Extent[1] < 5) && (kBox.Extent[2] < 5) )
        	{
        		left_right_markers.remove(i);
        		lr_found = false;
        	}
            if (lr_found)
            {
//            	TriMesh mesh = createMesh( image, mask );
            	leftRightMask.or(mask);
            }
            else if ( head_found )
            {
				headMasks.add(mask);
				headMaskPoints.add(maskPoints);
            }
    	}    	
//    	System.err.println( "   fillMarkers " + left_right_markers.size() );
    }

    private void findLeftRightMarkers(ModelImage image, Vector3f min, Vector3f max)
    {
    	int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
    	int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
    	int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;    
		image.resetVOIs();
    	float markerVal = max_LR;
    	
    	float pixelSize = 0.1625f;
    	float markerDiameter = 3f;

//    	int cubeSize = 22;//(int) Math.floor(markerDiameter/pixelSize);
    	int cubeSize = 11;//(int) Math.floor(markerDiameter/pixelSize);
    	int cubeHalf = cubeSize/2;

//    	Vector<VOIContour> latticeResults = new Vector<VOIContour>();
    	VOIContour centers = findMarkers(image,markerVal, cubeSize, 20, min, max);  			
//		latticeResults.add( centers );
		
//    	while ( (cubeSize > 15) )
//    	{
//    		cubeSize--;
//    		cubeHalf = cubeSize/2;
//    		centers = findMarkers(image, markerVal, cubeSize, 20, min, max);    		
//    		latticeResults.add( centers );
//    	}
    	
    	
//    	int closest = -1;
//    	float minDif = Float.MAX_VALUE;
//    	for ( int i = 0; i < latticeResults.size(); i++ )
//    	{
//    		float dif = Math.abs( latticeResults.elementAt(i).size() - 20 );
//    		if ( dif < minDif )
//    		{
//    			minDif = dif;
//    			closest = i;
//    		}
//    	}
//    	if ( closest != -1 )
//    	{
//    		left_right_markers = new Vector<Vector3f>();
//        	VOIContour seedPoints = latticeResults.elementAt(closest);
//        	for ( int i = 0; i < seedPoints.size(); i++ )
//        	{
//        		Vector3f seed = seedPoints.elementAt(i);    		
//        		left_right_markers.add(new Vector3f(seed));
//        	}
//        	saveLeftRightMarkers(image);
//    	}
    	float minDist = Float.MAX_VALUE;
    	int minI = -1, minJ = -1;
		left_right_markers = new Vector<Vector3f>();
		
    	for ( int i = 0; i < centers.size(); i++ )
    	{
    		Vector3f seed = centers.elementAt(i); 
    		left_right_markers.add(new Vector3f(seed));
//    		System.err.println( seed );
    		for ( int j = i+1; j < centers.size(); j++ )
    		{
    			float distance = centers.elementAt(i).distance(centers.elementAt(j) );
    			if ( distance < minDist );
    			{
    				minDist = distance;
    				minI = i;
    				minJ = j;
    			}
    		}
    	}
//    	if ( minI != -1 )
//    		System.err.println( minDist + "      " + centers.elementAt(minI) + "      " + centers.elementAt(minJ) );
    }

    private VOIContour findMarkers( ModelImage image, float intensityMin, int diameter, int maxDiameter, Vector3f min, Vector3f max )
    {
    	int cubeSize = diameter;
    	int cubeHalf = cubeSize/2;
    	int cubeThird = cubeSize/3;
    	
//    	System.err.println( cubeSize + " " + cubeHalf + " " + cubeThird );
    	
    	
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
    				if ( value > intensityMin )
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
    				
    				if ( value > intensityMin )
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
    					
//    					if ( testMidLine(centerP) )
//    					{
//    						continue;
//    					}
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
//        					if ( meanIntensity >= 0.95*intensityMin )
    					{
//    						count = 0;
//    						double meanVariance = 0;
//        					for ( int z2 = z - cubeHalf; z2 < (z+cubeHalf); z2++ )
//        					{
//        						for ( int y2 = y - cubeHalf; y2 < (y+cubeHalf); y2++ )
//        						{
//        							for ( int x2 = x - cubeHalf; x2 < (x+cubeHalf); x2++ )
//    								{
//    									if ( (x2 < dimX) && (y2 < dimY) && (z2 < dimZ) )
//    									{
//    										testP.set(x2,y2,z2);
//    										if ( centerP.distance(testP) <= cubeHalf )
//    										{
//    											value = image.getFloat(x2,y2,z2);
//    											meanVariance += ((value - meanIntensity) * (value - meanIntensity));
//    											count++;
//    										}
//    									}
//    								}
//    							}    					
//    						}
//    						meanVariance /= count;
//    						minVar = Math.min( minVar, meanVariance);
//    						maxVar = Math.max( maxVar, meanVariance);
//    						cubeIntensities.add(new Vector2d( meanIntensity, meanVariance) );

    						Vector3f center = new Vector3f( x, y, z );
    						cubeCenters.add(center);
    					}
    				}
    			}
    		}
    	}
    	
//    	System.err.println( "minimum variance = " + minVar + "   maximum variance = " + maxVar );
//    	double varCutoff = minVar + 0.03 * (maxVar - minVar);
//    	System.err.println( "Number centers = " + cubeCenters.size() );
    	

    	Vector<VOIContour> clusterList = makeClusters( cubeCenters, cubeSize, maxDiameter );
    	
    	VOIContour centerPoints = new VOIContour(false);
//    	System.err.println( "Clusters = " + clusterList.size() );
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

//    	cubeIntensities.clear();
//    	cubeIntensities = null;
    	cubeCenters.clear();
    	cubeCenters = null;
    	return centerPoints;
    }

    private void findMidline(ModelImage image, Vector3f min, Vector3f max)
    {
    	
    	float markerVal = max_head;
//    	System.err.println( "image max = " + image.getMax() + " 99% = " + max_98P );

    	
    	float pixelSize = 0.1625f;
    	float markerDiameter = 3f;
    	
    	int cubeSize = 5;//(int) Math.floor(markerDiameter/pixelSize);
    	int cubeHalf = cubeSize/2;

    	Vector<Vector3f> centers = findMarkers(image,markerVal, cubeSize, cubeSize, min, max); 
    	Vector<Vector3f> averages = new Vector<Vector3f>();
    	Vector<VOIContour> lines = new Vector<VOIContour>();
    	Vector<VOIContour> endPoints = new Vector<VOIContour>();
    	Vector<VOIContour> inflectionPoints = new Vector<VOIContour>();
    	Vector<VOIContour> clusters = makeClusters( image, markerVal, centers, averages, lines, endPoints, inflectionPoints, midLineClusterDistance, midLineMaxDiameter );
//    	System.err.println( centers.size() + "   " + averages.size() + "   " + lines.size() );

    	short id = (short) image.getVOIs().getUniqueID();

//    	Color[] colorList = new Color[]{Color.red,Color.blue,Color.green,Color.yellow,Color.cyan, Color.magenta, 
//    			Color.orange, Color.pink, Color.white };
//    	ColorRGBA[] colorListR = new ColorRGBA[]{
//    			new ColorRGBA(1,0,0,1),
//    			new ColorRGBA(0,0,1,1),
//    			new ColorRGBA(0,1,0,1),
//    			new ColorRGBA(1,1,0,1),
//    			new ColorRGBA(0,1,1,1),
//    			new ColorRGBA(1,0,1,1),
//    			new ColorRGBA(1,.5f,0,1),
//    			new ColorRGBA(1,.5f,.5f,1),
//    			new ColorRGBA(1,1,1,1) };
//
//    	int fileCount = 0;
//    	int colorCount = 0;
////    	System.err.println( lines.size() );
//    	for ( int i = 0; i < lines.size(); i++ )
//    	{
//    		Color useColor = colorList[i%(colorList.length-1)];
//    		ColorRGBA useColorRGBA = colorListR[i%(colorList.length-1)];
//    		
//            Box3f kBox = ContBox3f.ContOrientedBox(lines.elementAt(i).size(), lines.elementAt(i));
////    		System.err.println( i + "   " + kBox.Extent[0] + " " + kBox.Extent[1] + " " + kBox.Extent[2] );
//    		float diff01 = Math.abs( kBox.Extent[0] - kBox.Extent[1] );
//    		float diff02 = Math.abs( kBox.Extent[0] - kBox.Extent[2] );
//    		float diff12 = Math.abs( kBox.Extent[1] - kBox.Extent[2] );
//    		
//    		if ( (diff01 < 10) && (diff02 < 10) && (diff12 < 10) )
//    		{
//        		useColor = colorList[colorList.length-1];
//        		useColorRGBA = colorListR[colorListR.length-1];
//        		continue;
//    		}
//    		else
//    		{
//        		useColor = colorList[colorCount%(colorList.length-1)];
//        		useColorRGBA = colorListR[colorCount%(colorList.length-1)];
//        		colorCount++;
//    		}
//    		
//        	image.resetVOIs();
//        	head = lines.elementAt(i);
////    		System.err.println( "Head Cluster size " + head.size() );
//    		for ( int j = 0; j < head.size(); j++ )
//    		{
//        		Vector3f pt = head.elementAt(j);    		
//    
//        		VOI marker = new VOI(id++, "marker_" + j, VOI.POINT, (float)Math.random() );
//        		marker.importPoint(pt);
//        		image.registerVOI(marker);
//        		marker.setColor( useColor );
//        		marker.getCurves().elementAt(0).update( useColorRGBA );
//        		
//        		
//        		Vector<Vector3f> dataPoints = new Vector<Vector3f>();
//        		dataPoints.add( pt );
//        		for ( int z = (int)pt.Z - 10; z < pt.Z + 10; z++ )
//        		{
//            		for ( int y = (int)pt.Y - 10; y < pt.Y + 10; y++ )
//            		{
//                		for ( int x = (int)pt.X - 10; x < pt.X + 10; x++ )
//                		{
//    						if ( (x >= 0) && (y >= 0) && (z >= 0) && (x < dimX) && (y < dimY) && (z < dimZ) )
//    						{
//    							float value = image.getFloat(x,y,z);
//    							if ( value > markerVal )
//    							{
//    								dataPoints.add( new Vector3f(x,y,z) );
//    							}
//    						}
//                		}
//            		}
//        		}
//        		VOI axisVOI = new VOI(id++, "image" + id, VOI.POLYLINE, (float)Math.random() );
//        		Vector3f axis = ellipsoidFit(image, axisVOI, dataPoints);
//        		image.registerVOI(axisVOI);
//    		}    		
//
//    		String voiDir = image.getImageDirectory() + JDialogBase.makeImageName( image.getImageName(), "") + File.separator;
//            File voiFileDir = new File(voiDir);
//            if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
//            } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
//            } else { // voiFileDir does not exist
//                voiFileDir.mkdir();
//            }
//    		voiDir = image.getImageDirectory() + JDialogBase.makeImageName( image.getImageName(), "") + File.separator +
//			"headVOIs_" + fileCount++ + File.separator;
////    		voiDir = image.getImageDirectory() + JDialogBase.makeImageName( image.getImageName(), "") + File.separator +
////        			"headVOIs_" + (colorCount-1) + File.separator;
//            voiFileDir = new File(voiDir);
//            if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
//            } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
//            } else { // voiFileDir does not exist
//                voiFileDir.mkdir();
//            }
//        	saveAllVOIsTo( voiDir, image );
//    	}

    	if ( lines.size() == 0 )
    	{
    		return;
    	}
		head = lines.elementAt(0);
		VOIContour headInflectionPoints = inflectionPoints.elementAt(0);
		headEndPoints = endPoints.elementAt(0);
		head.addAll(headInflectionPoints);
		head.addAll(headEndPoints);

//		findEndPoints(image, head, headEndPoints, headInflectionPoints);
		
		
		
		
//		int hIndex = 0;
////    	for ( int hIndex = 0; hIndex < lines.size(); hIndex++ )
//    	{
//    		head = lines.elementAt(hIndex);
//    		VOIContour headEndPoints = endPoints.elementAt(hIndex);
//    		VOIContour headInflectionPoints = inflectionPoints.elementAt(hIndex);
//
//    		if ( head != null )
//    		{
//    			image.resetVOIs();
//    			int markerCount = 0;
//    			for ( int i = 0; i < headEndPoints.size(); i++ )
//    			{
//    				Vector3f pt = headEndPoints.elementAt(i);    		
//    				if ( head.contains(pt) )
//    				{
//    					head.remove(pt);
//    				}
//
//    				VOI marker = new VOI(id++, "marker_" + markerCount++, VOI.POINT, (float)Math.random() );
//    				marker.importPoint(pt);
//    				image.registerVOI(marker);
//    				marker.setColor( Color.blue );
//    				marker.getCurves().elementAt(0).update( new ColorRGBA(0, 0, 1, 1));
//    			}    		
//    			for ( int i = 0; i < headInflectionPoints.size(); i++ )
//    			{
//    				Vector3f pt = headInflectionPoints.elementAt(i);    		
//    				if ( head.contains(pt) )
//    				{
//    					head.remove(pt);
//    				}
//
//    				VOI marker = new VOI(id++, "marker_" + markerCount++, VOI.POINT, (float)Math.random() );
//    				marker.importPoint(pt);
//    				image.registerVOI(marker);
//    				marker.setColor( Color.green );
//    				marker.getCurves().elementAt(0).update( new ColorRGBA(0, 1, 0, 1));
//    			}    		
//    			//    		System.err.println( "Head Cluster size " + head.size() );
//    			for ( int i = 0; i < head.size(); i++ )
//    			{
//    				Vector3f pt = head.elementAt(i);    		
//
//    				VOI marker = new VOI(id++, "marker_" + markerCount++, VOI.POINT, (float)Math.random() );
//    				marker.importPoint(pt);
//    				image.registerVOI(marker);
//    				marker.setColor( Color.red );
//    				marker.getCurves().elementAt(0).update( new ColorRGBA(1, 0, 0, 1));
//    			}
//
//    			String voiDir = image.getImageDirectory() + JDialogBase.makeImageName( image.getImageName(), "") + File.separator;
//    			File voiFileDir = new File(voiDir);
//    			if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
//    			} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
//    			} else { // voiFileDir does not exist
//    				voiFileDir.mkdir();
//    			}
//    			voiDir = image.getImageDirectory() + JDialogBase.makeImageName( image.getImageName(), "") + File.separator +
//    					"headVOIs_" + hIndex + File.separator;
//    			voiFileDir = new File(voiDir);
//    			if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
//    			} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
//    			} else { // voiFileDir does not exist
//    				voiFileDir.mkdir();
//    			}
//    			saveAllVOIsTo( voiDir, image );
//    		}
//    		head.addAll(headEndPoints);
//    		head.addAll(headInflectionPoints);
//    	}
//		head = lines.elementAt(0);
//		headEndPoints = endPoints.elementAt(0);
    }
    private float findShortestPath( ModelImage image, Vector3f start, Vector3f end, VOIContour path )
    {
    	int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
    	int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
    	int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;    
    	
    	int startIndex = (int)(start.Z * dimX*dimY + start.Y*dimX + start.X);
    	int endIndex = (int)(end.Z * dimX*dimY + end.Y*dimX + end.X);
    	
        /* Initialize the vertex and index arrays, edge lists and weights for
         * the search: */
        initializePath(image, start, end);
        
        /* set the start point for the search: */
        int iSmallest = startIndex;
        weight[startIndex] = 0;

        int iCount = 1;
        
        Vector3f smallestPt = new Vector3f(start);

        /* Loop until the end point is reached: */
        while ((iSmallest != -1) && (iSmallest != endIndex)) {

            /* Relax the edges around the current vertex: */
            relaxEdges( image, smallestPt, iSmallest);

            /* Find the vertex with the smallest weight and add it to the
             * path: */
            smallestPt = findSmallest(image);
            if ( smallestPt == null )
            {
            	return Float.MAX_VALUE;
            }
            iSmallest = (int)(smallestPt.Z*dimX*dimY + smallestPt.Y*dimX + smallestPt.X);
        }

        /* Smoothes and stores the shortest path: */
        return createPath( image, startIndex, endIndex, path );
    }
    
    


    /**
     * The findSmallest function searches through the list of vertices that are not yet relaxed but that have been
     * visited by the Dijkstra Search, so that the weight factor is not Float.MAX_VALUE, but also is not the minimum
     * value for that node. In Dijkstra's search the vertex with the smallest weight is relaxed first, a greedy
     * algorithm that chooses the locally closest vertex to add to the path. The final weight for the vertex is
     * determined when it is relaxed.
     *
     * <p>This function uses the data member m_kBorder -- the linked list of vertices that have been visited by
     * Dijkstra's search, but which have not yet been relaxed.</p>
     *
     * @return  int index of the vertex with the smallest weight
     */
    private Vector3f findSmallest( ModelImage image ) {

        /* Get the number of vertices so we can search the entire border
         * list:*/
        int iSize = border.size();

        if (iSize == 0) {
            return null;
        }
    	int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
    	int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;

        Vector3f currentPt;
        Vector3f smallest = border.elementAt(0);
        int iSmallest = (int)(smallest.Z*dimX*dimY + smallest.Y*dimX + smallest.X);
        int smallestBorder = 0;

        /* Loop over all the vertices in the border: */
        for (int iBorder = 0; iBorder < iSize; iBorder++) {
        	currentPt = border.elementAt(iBorder);
        	
        	int index = (int)(currentPt.Z*dimX*dimY + currentPt.Y*dimX + currentPt.X);

            /* If the weight of the vertex, plus the remaining weight factor
             * is less then the current smallest weight, set the smallest to
             * be that vertex: */
            if ((weight[index] + remainingWeight[index]) <
                    (weight[iSmallest] + remainingWeight[iSmallest])) {
                iSmallest = index;
                smallestBorder = iBorder;
            }
        }

        /* Return the index of the vertex with the smallest weight: */
        return border.elementAt(smallestBorder);
    }
    
    
    
    
    private void floodFill(ModelImage image, BitSet mask, Vector3f cog)
    {
    	
    	int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
    	int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
    	int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;   
    	
    	int length = dimX * dimY * dimZ;
    	BitSet visited = new BitSet(length);
    	BitSet whiteMatter = new BitSet(length);
    	
    	Vector<Vector3f> seedList = new Vector<Vector3f>();
    	seedList.add(cog);
		int index = (int) (cog.Z * dimX * dimY + cog.Y * dimX + cog.X);	
		visited.set(index);
		whiteMatter.set(index);
		
    	
		fill( image, seedList, visited, mask );

		visited.clear();
		visited = null;
		whiteMatter.clear();
		whiteMatter = null;
    	
    }
    
    /**
     * Compute the point of intersection between a line (0,iY,iZ)+t(1,0,0) and
     * the triangle defined by the three input points. All calculations are in
     * voxel coordinates and the x-value of the intersection point is
     * truncated to an integer.
     *
     * @param   kV0  a 3D vertex of the triangle
     * @param   kV1  a 3D vertex of the triangle
     * @param   kV2  a 3D vertex of the triangle
     * @param   iY   the y-value of the origin of the line
     * @param   iZ   the z-value of the origin of the line
     *
     * @return  the x-value of the intersection
     */
    private float getIntersectX(Vector3f kV0, Vector3f kV1, Vector3f kV2,
                                  float iY, float iZ) {

        // Compute the intersection, if any, by calculating barycentric
        // coordinates of the intersection of the line with the plane of
        // the triangle.  The barycentric coordinates are K0 = fC0/fDet,
        // K1 = fC1/fDet, and K2 = fC2/fDet with K0+K1+K2=1.  The intersection
        // point with the plane is K0*V0+K1*V1+K2*V2.  The point is inside
        // the triangle whenever K0, K1, and K2 are all in the interval [0,1].
        float fPu = iY - kV0.Y, fPv = iZ - kV0.Z;
        float fE1u = kV1.Y - kV0.Y, fE1v = kV1.Z - kV0.Z;
        float fE2u = kV2.Y - kV0.Y, fE2v = kV2.Z - kV0.Z;
        float fE1dP = (fE1u * fPu) + (fE1v * fPv);
        float fE2dP = (fE2u * fPu) + (fE2v * fPv);
        float fE1dE1 = (fE1u * fE1u) + (fE1v * fE1v);
        float fE1dE2 = (fE1u * fE2u) + (fE1v * fE2v);
        float fE2dE2 = (fE2u * fE2u) + (fE2v * fE2v);
        float fDet = Math.abs((fE1dE1 * fE2dE2) - (fE1dE2 * fE1dE2));

        float fC1 = (fE2dE2 * fE1dP) - (fE1dE2 * fE2dP);

        if ((fC1 < 0.0f) || (fC1 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        float fC2 = (fE1dE1 * fE2dP) - (fE1dE2 * fE1dP);

        if ((fC2 < 0.0f) || (fC2 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        float fC0 = fDet - fC1 - fC2;

        if ((fC0 < 0.0f) || (fC0 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        if (fDet == 0) {
            return -1;
        }

        return ((fC0 * kV0.X) + (fC1 * kV1.X) + (fC2 * kV2.X)) / fDet;
    }




    /**
     * Compute the point of intersection between a line (iX,0,iZ)+t(0,1,0) and
     * the triangle defined by the three input points. All calculations are in
     * voxel coordinates and the y-value of the intersection point is
     * truncated to an integer.
     *
     * @param   kV0  a 3D vertex of the triangle
     * @param   kV1  a 3D vertex of the triangle
     * @param   kV2  a 3D vertex of the triangle
     * @param   iX   the x-value of the origin of the line
     * @param   iZ   the z-value of the origin of the line
     *
     * @return  the y-value of the intersection
     */
    private float getIntersectY(Vector3f kV0, Vector3f kV1, Vector3f kV2,
                                  float iX, float iZ) {

        // Compute the intersection, if any, by calculating barycentric
        // coordinates of the intersection of the line with the plane of
        // the triangle.  The barycentric coordinates are K0 = fC0/fDet,
        // K1 = fC1/fDet, and K2 = fC2/fDet with K0+K1+K2=1.  The intersection
        // point with the plane is K0*V0+K1*V1+K2*V2.  The point is inside
        // the triangle whenever K0, K1, and K2 are all in the interval [0,1].
        float fPu = iX - kV0.X, fPv = iZ - kV0.Z;
        float fE1u = kV1.X - kV0.X, fE1v = kV1.Z - kV0.Z;
        float fE2u = kV2.X - kV0.X, fE2v = kV2.Z - kV0.Z;
        float fE1dP = (fE1u * fPu) + (fE1v * fPv);
        float fE2dP = (fE2u * fPu) + (fE2v * fPv);
        float fE1dE1 = (fE1u * fE1u) + (fE1v * fE1v);
        float fE1dE2 = (fE1u * fE2u) + (fE1v * fE2v);
        float fE2dE2 = (fE2u * fE2u) + (fE2v * fE2v);
        float fDet = Math.abs((fE1dE1 * fE2dE2) - (fE1dE2 * fE1dE2));

        float fC1 = (fE2dE2 * fE1dP) - (fE1dE2 * fE2dP);

        if ((fC1 < 0.0f) || (fC1 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        float fC2 = (fE1dE1 * fE2dP) - (fE1dE2 * fE1dP);

        if ((fC2 < 0.0f) || (fC2 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        float fC0 = fDet - fC1 - fC2;

        if ((fC0 < 0.0f) || (fC0 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        if (fDet == 0) {
            return -1;
        }

        return ((fC0 * kV0.Y) + (fC1 * kV1.Y) + (fC2 * kV2.Y)) / fDet;
    }

    /**
     * Compute the point of intersection between a line (iX,iY,0)+t(0,0,1) and
     * the triangle defined by the three input points. All calculations are in
     * voxel coordinates and the z-value of the intersection point is
     * truncated to an integer.
     *
     * @param   kV0  a 3D vertex of the triangle
     * @param   kV1  a 3D vertex of the triangle
     * @param   kV2  a 3D vertex of the triangle
     * @param   iX   the x-value of the origin of the line
     * @param   iY   the y-value of the origin of the line
     *
     * @return  the z-value of the intersection
     */
    private float getIntersectZ(Vector3f kV0, Vector3f kV1, Vector3f kV2,
                                  float iX, float iY) {

        // Compute the intersection, if any, by calculating barycentric
        // coordinates of the intersection of the line with the plane of
        // the triangle.  The barycentric coordinates are K0 = fC0/fDet,
        // K1 = fC1/fDet, and K2 = fC2/fDet with K0+K1+K2=1.  The intersection
        // point with the plane is K0*V0+K1*V1+K2*V2.  The point is inside
        // the triangle whenever K0, K1, and K2 are all in the interval [0,1].
        float fPu = iX - kV0.X, fPv = iY - kV0.Y;
        float fE1u = kV1.X - kV0.X, fE1v = kV1.Y - kV0.Y;
        float fE2u = kV2.X - kV0.X, fE2v = kV2.Y - kV0.Y;
        float fE1dP = (fE1u * fPu) + (fE1v * fPv);
        float fE2dP = (fE2u * fPu) + (fE2v * fPv);
        float fE1dE1 = (fE1u * fE1u) + (fE1v * fE1v);
        float fE1dE2 = (fE1u * fE2u) + (fE1v * fE2v);
        float fE2dE2 = (fE2u * fE2u) + (fE2v * fE2v);
        float fDet = Math.abs((fE1dE1 * fE2dE2) - (fE1dE2 * fE1dE2));

        float fC1 = (fE2dE2 * fE1dP) - (fE1dE2 * fE2dP);

        if ((fC1 < 0.0f) || (fC1 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        float fC2 = (fE1dE1 * fE2dP) - (fE1dE2 * fE1dP);

        if ((fC2 < 0.0f) || (fC2 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        float fC0 = fDet - fC1 - fC2;

        if ((fC0 < 0.0f) || (fC0 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        if (fDet == 0) {
            return -1;
        }

        return ((fC0 * kV0.Z) + (fC1 * kV1.Z) + (fC2 * kV2.Z)) / fDet;
    }
    private void growLattice( Vector<VOI> latticeList, Vector<Vector2d> costList,
    		VOI lattice,  Vector<Vector3f> markers )
    {
		Vector<Vector3f> leftSide = lattice.getCurves().elementAt(0);
		Vector<Vector3f> rightSide = lattice.getCurves().elementAt(1);
    	if ( leftSide.size() >= 7 )
		{
			Vector2d cost = costFunction( leftSide, rightSide, false, false );
			if ( cost.X < Float.MAX_VALUE )
			{
				latticeList.add( lattice );
				cost.Y = latticeList.size() - 1;
				costList.add(cost);
				return;
			}
		}
    	
//    	if ( leftSide.size() == 6 )
//    	{
//    		System.err.println( "adding last pair" );
//    	}
    	
    	int[][] potentialPairs = new int[markers.size()][markers.size()];
    	for ( int i = 0; i < markers.size(); i++ )
    	{
    		for ( int j = 0; j < markers.size(); j++ )
    		{
    			potentialPairs[i][j] = -1;
    		}
    	}
    	for ( int i = 0; i < markers.size(); i++ )
    	{
    		for ( int j = 0; j < markers.size(); j++ )
    		{
    			if ( i != j )
    			{
    				float distance = markers.elementAt(i).distance( markers.elementAt(j) );
    				if ( distance <= 2*midLineMarkerDistance )
    				{
						potentialPairs[i][j] = j;
    				}
    			}
    		}
    	}
    	Vector<VOIContour> pairs = new Vector<VOIContour>();
		for ( int i = 0; i < potentialPairs.length; i++ )
		{
			for ( int j = 0; j < potentialPairs.length; j++ )
			{
				if ( (potentialPairs[i][j] != -1) && (potentialPairs[i][j] == j) && (potentialPairs[j][i] == i) )
				{
//					System.err.println( "Pair " + i + " " + j );
					
					VOIContour pair = new VOIContour(false);
					pair.add( new Vector3f( markers.elementAt(i) ) );
					pair.add( new Vector3f( markers.elementAt(j) ) );
					pairs.add(pair);
				}
			}
		}
//		System.err.println( pairs.size() );
		for ( int i = pairs.size()-1; i >= 0; i-- )
		{
			VOIContour pairI = pairs.elementAt(i);
			for ( int j = i-1; j >= 0; j-- )
			{
				VOIContour pairJ = pairs.elementAt(j);
				if ( pairI.firstElement().equals( pairJ.firstElement() ) && pairI.lastElement().equals( pairJ.lastElement() ) )
				{
					pairs.remove(i);
					break;
				}
				if ( pairI.firstElement().equals( pairJ.lastElement() ) && pairI.lastElement().equals( pairJ.firstElement() ) )
				{
					pairs.remove(i);
					break;
				}
			}
		}
//		System.err.println( "Pairs = " + pairs.size() );


		Vector3f lastLeft = leftSide.lastElement();
		Vector3f lastRight = rightSide.lastElement();
    	float currentWidth = lastLeft.distance(lastRight);
		for ( int i = pairs.size()-1; i >= 0; i-- )
		{
			float distanceL = lastLeft.distance( pairs.elementAt(i).firstElement() );
			float distanceR = lastRight.distance( pairs.elementAt(i).lastElement() );
			
			if ( (distanceL > 150) || (distanceR > 150) )
			{
				pairs.remove(i);
			}
		}
		
		for ( int i = pairs.size()-1; i >= 0; i-- )
		{
			float distance = pairs.elementAt(i).firstElement().distance( pairs.elementAt(i).lastElement() );
			if ( distance > 1.2 * currentWidth )
			{
				pairs.remove(i);
			}
		}
		
		Vector<VOI> newLattices = new Vector<VOI>();
		
		int minIndex = -1;
		double minCost = Double.MAX_VALUE;
		boolean reverse = false;
		for ( int i = 0; i < pairs.size(); i++ )
		{
			if ( isBox( lastLeft, lastRight, pairs.elementAt(i).firstElement(), pairs.elementAt(i).lastElement() ) )
			{
				leftSide.add( pairs.elementAt(i).firstElement() );
				rightSide.add( pairs.elementAt(i).lastElement() );	
				Vector2d cost = costFunction( leftSide, rightSide, false, false );

				if ( cost.X < minCost )
				{
					minCost = cost.X;
					minIndex = i;
					reverse = false;
				}
				leftSide.remove( pairs.elementAt(i).firstElement() );
				rightSide.remove( pairs.elementAt(i).lastElement() );	
			}
			else if ( isBox( lastLeft, lastRight, pairs.elementAt(i).lastElement(), pairs.elementAt(i).firstElement() ) )
			{
				leftSide.add( pairs.elementAt(i).lastElement() );
				rightSide.add( pairs.elementAt(i).firstElement() );	
				Vector2d cost = costFunction( leftSide, rightSide, false, false );

				if ( cost.X < minCost )
				{
					minCost = cost.X;
					minIndex = i;
					reverse = true;
				}
				leftSide.remove( pairs.elementAt(i).lastElement() );
				rightSide.remove( pairs.elementAt(i).firstElement() );	
			}			
		}
		if ( minIndex != -1 )
		{
			if ( !reverse )
			{
				leftSide.add( pairs.elementAt(minIndex).firstElement() );
				rightSide.add( pairs.elementAt(minIndex).lastElement() );			
			}
			else
			{				
				leftSide.add( pairs.elementAt(minIndex).lastElement() );
				rightSide.add( pairs.elementAt(minIndex).firstElement() );	
			}
			newLattices.add(lattice);
		}
//		System.err.println( "Lattice size = " + leftSide.size() + " number of new lattices: " + newLattices.size() );
		if ( newLattices.size() > 0 )
		{
			for ( int i = 0; i < newLattices.size(); i++ )
			{
				Vector<Vector3f> newMarkers = new Vector<Vector3f>();
				newMarkers.addAll(markers);
				newMarkers.removeAll( newLattices.elementAt(i).getCurves().elementAt(0) );
				newMarkers.removeAll( newLattices.elementAt(i).getCurves().elementAt(1) );
				growLattice( latticeList, costList, newLattices.elementAt(i), newMarkers );
			}
		}
		else
		{
			Vector2d cost = costFunction( leftSide, rightSide, false, false );
			if ( cost.X < Float.MAX_VALUE )
			{
				latticeList.add( lattice );
				cost.Y = latticeList.size() - 1;
				costList.add(cost);
			}
		}
		
    }
    
    
    private void growLattice2( Vector<VOI> latticeList, Vector<Vector2d> costList,
    		VOI lattice,  Vector<Vector3f> markers )
    {
    	int[][] potentialPairs = new int[markers.size()][markers.size()];
    	for ( int i = 0; i < markers.size(); i++ )
    	{
    		for ( int j = 0; j < markers.size(); j++ )
    		{
    			potentialPairs[i][j] = -1;
    		}
    	}
    	for ( int i = 0; i < markers.size(); i++ )
    	{
    		for ( int j = 0; j < markers.size(); j++ )
    		{
    			if ( i != j )
    			{
    				float distance = markers.elementAt(i).distance( markers.elementAt(j) );
    				if ( distance <= 2*midLineMarkerDistance )
    				{
						potentialPairs[i][j] = j;
    				}
    			}
    		}
    	}
    	Vector<VOIContour> pairs = new Vector<VOIContour>();
		for ( int i = 0; i < potentialPairs.length; i++ )
		{
			for ( int j = 0; j < potentialPairs.length; j++ )
			{
				if ( (potentialPairs[i][j] != -1) && (potentialPairs[i][j] == j) && (potentialPairs[j][i] == i) )
				{
//					System.err.println( "Pair " + i + " " + j );
					
					VOIContour pair = new VOIContour(false);
					pair.add( new Vector3f( markers.elementAt(i) ) );
					pair.add( new Vector3f( markers.elementAt(j) ) );
					pairs.add(pair);
				}
			}
		}
//		System.err.println( pairs.size() );
		for ( int i = pairs.size()-1; i >= 0; i-- )
		{
			VOIContour pairI = pairs.elementAt(i);
			for ( int j = i-1; j >= 0; j-- )
			{
				VOIContour pairJ = pairs.elementAt(j);
				if ( pairI.firstElement().equals( pairJ.firstElement() ) && pairI.lastElement().equals( pairJ.lastElement() ) )
				{
					pairs.remove(i);
					break;
				}
				if ( pairI.firstElement().equals( pairJ.lastElement() ) && pairI.lastElement().equals( pairJ.firstElement() ) )
				{
					pairs.remove(i);
					break;
				}
			}
		}
//		System.err.println( "Pairs = " + pairs.size() );
		
		
		Vector<Vector3f> leftSide = lattice.getCurves().elementAt(0);
		Vector<Vector3f> rightSide = lattice.getCurves().elementAt(1);
		Vector3f lastLeft = leftSide.lastElement();
		Vector3f lastRight = rightSide.lastElement();
		Vector<VOI> newLattices = new Vector<VOI>();
		for ( int i = 0; i < pairs.size(); i++ )
		{
			if ( isBox( lastLeft, lastRight, pairs.elementAt(i).firstElement(), pairs.elementAt(i).lastElement() ) )
			{
				leftSide.add( pairs.elementAt(i).firstElement() );
				rightSide.add( pairs.elementAt(i).lastElement() );
				Vector2d cost = costFunction( leftSide, rightSide, false, false );
				if ( cost.X < Float.MAX_VALUE )
				{
					VOI newLattice = new VOI(lattice);
					newLattices.add(newLattice);
				}
				leftSide.remove( leftSide.size()-1);
				rightSide.remove( rightSide.size()-1);
			}
			else if ( isBox( lastLeft, lastRight, pairs.elementAt(i).lastElement(), pairs.elementAt(i).firstElement() ) )
			{
				leftSide.add( pairs.elementAt(i).lastElement() );
				rightSide.add( pairs.elementAt(i).firstElement() );
				Vector2d cost = costFunction( leftSide, rightSide, false, false );
				if ( cost.X < Float.MAX_VALUE )
				{
					VOI newLattice = new VOI(lattice);
					newLattices.add(newLattice);
				}				
				leftSide.remove( leftSide.size()-1);
				rightSide.remove( rightSide.size()-1);
			}			
		}
//		System.err.println( "Lattice size = " + leftSide.size() + " number of new lattices: " + newLattices.size() );
		if ( newLattices.size() > 0 )
		{
			for ( int i = 0; i < newLattices.size(); i++ )
			{
				Vector<Vector3f> newMarkers = new Vector<Vector3f>();
				newMarkers.addAll(markers);
				newMarkers.removeAll( newLattices.elementAt(i).getCurves().elementAt(0) );
				newMarkers.removeAll( newLattices.elementAt(i).getCurves().elementAt(1) );
				growLattice( latticeList, costList, newLattices.elementAt(i), newMarkers );
			}
		}
		else
		{
			Vector2d cost = costFunction( leftSide, rightSide, false, false );
			if ( cost.X < Float.MAX_VALUE )
			{
				latticeList.add( lattice );
				cost.Y = latticeList.size() - 1;
				costList.add(cost);
			}
		}
		
    }
    
    
    
    
    
    
	private void initializePath( ModelImage image, Vector3f start, Vector3f end )
    {
    	int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
    	int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
    	int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;    
    	
    	
    	int length = dimX * dimY * dimZ;


        /* The touched, but not relaxed vertices for Dijkstra's search: */
    	border = new Vector<Vector3f>();

        /* Optimization weight: straight-line distance from this vertex to end
         * vertex: */
    	remainingWeight = new float[length];

        /* Path length along edges from start vertex to this vertex:*/
    	weight = new float[length];

        /* Whether or not this vertex is relaxed: */
    	relaxed = new boolean[length];

        /* The vertex on the path before this one: */
        previous = new int[length];
    	

    	Vector3f temp = new Vector3f();
    	for ( int z = 0; z < dimZ; z++ )
    	{
    		for ( int y = 0; y < dimY; y++ )
    		{
    			for ( int x = 0; x < dimX; x++ )
    			{
			    	int index = (z*dimX*dimY + y*dimX + x);
    				if ( image.getMask().get(index) )
    				{
    			    	temp.set(x,y,z);
    			    	float distance = end.distance(temp);
    			    	/* Initialize search data variables: */
    			    	remainingWeight[index] = 100 * distance;
    			    	weight[index] = Float.MAX_VALUE;
    			    	relaxed[index] = false;
    			    	previous[index] = -1;
    				}
    			}
    		}
        }
    }
	private void initLattice( ModelImage image, float intensityMin, int maxDiameter )
    {    	
		if ( headPoints.size() == 0 )
		{
			System.err.println( " ... lattice initialization failed" );
			return;
		}
		
		while ( left_right_markers.size() > 22 )
		{
			float minVal = Float.MAX_VALUE;
			int minIndex = -1;
			for ( int i = left_right_markers.size() -1; i >= 0; i-- )
			{
				Vector3f pt = left_right_markers.elementAt(i);
				float value = image.getFloatTriLinearBounds( pt.X, pt.Y, pt.Z );
				if ( value < minVal )
				{
					minVal = value;
					minIndex = i;
				}
			}
			if ( minIndex != -1 )
			{
				left_right_markers.remove(minIndex);
			}
			else
			{
				break;
			}
		}
    	
		Vector<Vector3f> nearPoints = new Vector<Vector3f>();
		Vector2d[] distances = new Vector2d[left_right_markers.size()];
		for ( int i = 0; i < left_right_markers.size(); i++ )
		{
			Vector3f minPt = new Vector3f();
			float minDist = Float.MAX_VALUE;
			for ( int j = 0; j < headPoints.size(); j++ )
			{
				float distance = left_right_markers.elementAt(i).distance(headPoints.elementAt(j) );
				if ( distance < minDist )
				{
					minDist = distance;
					minPt.copy( headPoints.elementAt(j) );
				}
			}
			nearPoints.add(minPt);
			distances[i] = new Vector2d( minDist, i );
		}
		
		Arrays.sort(distances);
		int count = 0;
		for ( int i = 0; i < distances.length; i++ )
		{
			if ( distances[i].X < midLineMarkerDistance )
			{
				count++;
			}
		}
		if ( count == 0 )
		{
			System.err.println( " ... no pairs found" );
			return;
		}
		
		Vector2d[] distances2 = new Vector2d[count];
		VOIContour headPairs = new VOIContour(false);
		count = 0;
		for ( int i = 0; i < distances.length; i++ )
		{
			if ( distances[i].X < midLineMarkerDistance )
			{
//				System.err.println( (int)distances[i].Y + " " + distances[i].X );
				headPairs.add( left_right_markers.elementAt( (int)distances[i].Y) );
				distances2[i] = new Vector2d( distances[i].X, count++ );
			}
		}
		

    	int[][] potentialPairs = new int[headPairs.size()][headPairs.size()];
    	for ( int i = 0; i < headPairs.size(); i++ )
    	{
    		for ( int j = 0; j < headPairs.size(); j++ )
    		{
    			potentialPairs[i][j] = -1;
    		}
    	}
    	
    	for ( int i = 0; i < headPairs.size(); i++ )
    	{
    		for ( int j = 0; j < headPairs.size(); j++ )
    		{
    			if ( i != j )
    			{
    				float distance = headPairs.elementAt(i).distance( headPairs.elementAt(j) );
    				if ( distance <= 2*midLineMarkerDistance )
    				{
    					Segment3f segment = new Segment3f( headPairs.elementAt(i),  headPairs.elementAt(j) );
    					for ( int k = 0; k < headPoints.size(); k++ )
    					{
    						DistanceVector3Segment3 distTest = new DistanceVector3Segment3( headPoints.elementAt(k), segment );
    						distance = distTest.Get();
    						if ( distance < 2*midLineClusterDistance )
    						{
    							Vector3f pt = headPoints.elementAt(k);
    							float d1 = pt.distance( headPairs.elementAt(i) );
    							float d2 = pt.distance( headPairs.elementAt(j) );
    							float dif = Math.abs( d1 - d2 );
    							if ( dif <= midLineClusterDistance )
    							{
    								potentialPairs[i][j] = j;
    								break;
    							}
    						}
    					}
    				}
    			}
    		}
    	}    	

		
		
		
		image.resetVOIs();
    	Vector<VOIContour> pairs = new Vector<VOIContour>();
		for ( int i = 0; i < potentialPairs.length; i++ )
		{
			for ( int j = 0; j < potentialPairs.length; j++ )
			{
				if ( (potentialPairs[i][j] != -1) && (potentialPairs[i][j] == j) && (potentialPairs[j][i] == i) )
				{
					VOIContour pair = new VOIContour(false);
					pair.add( new Vector3f( headPairs.elementAt(i) ) );
					pair.add( new Vector3f( headPairs.elementAt(j) ) );
					pairs.add(pair);
				}
			}
		}
		
		for ( int i = pairs.size()-1; i >= 0; i-- )
		{
			VOIContour pairI = pairs.elementAt(i);
			for ( int j = i-1; j >= 0; j-- )
			{
				VOIContour pairJ = pairs.elementAt(j);
				if ( pairI.firstElement().equals( pairJ.firstElement() ) && pairI.lastElement().equals( pairJ.lastElement() ) )
				{
					pairs.remove(i);
					break;
				}
				if ( pairI.firstElement().equals( pairJ.lastElement() ) && pairI.lastElement().equals( pairJ.firstElement() ) )
				{
					pairs.remove(i);
					break;
				}
			}
		}

		
//		System.err.println( "Pairs = " + pairs.size() );
		for ( int i = pairs.size() - 1; i >= 0; i-- )
		{
			boolean box = false;
			for ( int j = pairs.size() - 1; j >= 0; j-- )
			{
				if ( i != j )
				{
					if ( isBox( pairs.elementAt(i).firstElement(), pairs.elementAt(i).lastElement(), 
							pairs.elementAt(j).firstElement(), pairs.elementAt(j).lastElement() ) )
					{
						box = true;
						break;
					}
					if ( isBox( pairs.elementAt(i).firstElement(), pairs.elementAt(i).lastElement(), 
							pairs.elementAt(j).lastElement(), pairs.elementAt(j).firstElement() ) )
					{
						box = true;
						break;
					}
				}				
			}
			if ( !box )
			{
				pairs.remove(i);
			}
		}
//		System.err.println( "Pairs = " + pairs.size() );
		if ( pairs.size() <= 0 )
		{
			System.err.println( " ... no pairs match" );
			return;
		}
		
		int pairsSize = pairs.size();
		while ( pairs.size() > 11 )
		{
			float maxDist = -1;
			int index = -1;
			for ( int i = pairs.size() -1; i >= 0; i-- )
			{
				float distance = pairs.elementAt(i).firstElement().distance( pairs.elementAt(i).lastElement() );
				if ( distance > maxDist )
				{
					maxDist = distance;
					index = i;
				}
			}
			if ( index == -1 )
			{
				break;
			}
			pairs.remove(index);
		}
		
		
//		int maxCombinations = factorial(pairs.size());
//		System.err.println( pairs.size() + " " + maxCombinations );
		int[][] combinations = buildCombinations( pairs.size() );
		System.err.println( "    Original pairs: " + pairsSize + " " + pairs.size() );

        Vector<Vector2d> latticeCosts = new Vector<Vector2d>();
		Vector<VOI> latticeLists = new Vector<VOI>();
		Vector3f lastLeft, lastRight;
		for ( int i = 0; i < combinations.length; i++ )
		{			
			short id = (short) image.getVOIs().getUniqueID();
			VOI lattice = new VOI(id, "lattice", VOI.POLYLINE, (float)Math.random() );
			VOIContour leftSide = new VOIContour( false );
			VOIContour rightSide = new VOIContour( false );
			lattice.getCurves().add(leftSide);		
			lattice.getCurves().add(rightSide);

			int index = combinations[i][0];
			leftSide.add( pairs.elementAt(index).firstElement() );
			rightSide.add( pairs.elementAt(index).lastElement() );
			for ( int j = 1; j < combinations[i].length; j++ )
			{
				lastLeft = leftSide.lastElement();
				lastRight = rightSide.lastElement();
				
				int nextIndex = combinations[i][j];
				VOIContour nextPair = pairs.elementAt(nextIndex);
				if ( isBox( lastLeft, lastRight, nextPair.firstElement(), nextPair.lastElement() ) )
				{
					leftSide.add( nextPair.firstElement() );
					rightSide.add( nextPair.lastElement() );					
				}
				else if ( isBox( lastLeft, lastRight, nextPair.lastElement(), nextPair.firstElement() ) )
				{
					leftSide.add( nextPair.lastElement() );
					rightSide.add( nextPair.firstElement() );					
				}
				else
				{
					break;
				}
			}
			if ( leftSide.size() > 1 )
			{
	    		Vector2d cost = costFunction( leftSide, rightSide, true, false );
	    		if ( cost.X < Float.MAX_VALUE )
	    		{
	    			boolean newLattice = true;
	    			for ( int j = 0; j < latticeLists.size(); j++ )
	    			{
	    				if ( checkDuplicates( lattice, latticeLists.elementAt(j) ) )
	    				{
	    					newLattice = false;
	    					break;
	    				}
	    			}
	    			if ( newLattice )
	    			{
	    				latticeLists.add(lattice);
	    				cost.Y = latticeLists.size() - 1;
	    				latticeCosts.add( cost );
	    			}
		    		else
		    		{
		    			leftSide.clear();
		    			leftSide = null;
		    			rightSide.clear();
		    			rightSide = null;
		    			lattice = null;
		    		}
		    	}
	    		else
	    		{
	    			leftSide.clear();
	    			leftSide = null;
	    			rightSide.clear();
	    			rightSide = null;
	    			lattice = null;
	    		}
			}
			else
			{
				leftSide.clear();
				leftSide = null;
				rightSide.clear();
				rightSide = null;
				lattice = null;
			}
		}

//		System.err.println( "Lattice found: " + latticeLists.size() );			
		int latticeCount = latticeLists.size();
//		System.err.println( "Good Lattices found: " + latticeCount );
		if ( latticeCount == 0 )
		{
			System.err.println( " ... no lattice found" );
			return;
		}

		Vector2d[] costResults = new Vector2d[latticeLists.size()];
		latticeCount = 0;
		int maxLength = -1;
		for ( int i = 0; i < latticeLists.size(); i++ )
		{
			if ( latticeLists.elementAt(i).getCurves().elementAt(0).size() > maxLength )
			{
				maxLength = latticeLists.elementAt(i).getCurves().elementAt(0).size();
			}
			Vector2d cost = latticeCosts.elementAt(i);
			costResults[latticeCount++] = cost;
		}
		Arrays.sort(costResults);
		

//		float maxDist = -Float.MAX_VALUE;
//		for ( int i = 0; i < Math.min( costResults.length, 1); i++ )
//		{
//
//			Vector2d cost = costResults[i];
//			if ( cost.X < 2 )
//			{
//				VOI lattice = latticeLists.elementAt( (int)cost.Y );
//				VOIContour leftSide = (VOIContour)lattice.getCurves().elementAt(0);
//				VOIContour rightSide = (VOIContour)lattice.getCurves().elementAt(1);
//				for ( int j = 0; j < leftSide.size(); j++ )
//				{
//					float distance = leftSide.elementAt(j).distance( rightSide.elementAt(j) );
//					if ( distance > maxDist )
//					{
//						maxDist = distance;
//					}
//				}
//			}
//		}
//		System.err.println( "   maximum pair distance " + maxDist );

		Vector<VOI> finalLatticeLists = new Vector<VOI>();
		Vector<Vector2d> finalLatticeCosts = new Vector<Vector2d>();
//		for ( int i = 0; i < costResults.length; i++ )
		for ( int i = 0; i < 1; i++ )
		{
			Vector2d cost = costResults[i];
			if ( cost.X >= 2 )
			{
				break;
			}
			VOI lattice = latticeLists.elementAt( (int)cost.Y );
			VOIContour leftSide = (VOIContour)lattice.getCurves().elementAt(0);
			VOIContour rightSide = (VOIContour)lattice.getCurves().elementAt(1);
			Vector<Vector3f> remainingMarkers = new Vector<Vector3f>();
			for ( int j = 0; j < left_right_markers.size(); j++ )
			{
				if ( !leftSide.contains(left_right_markers.elementAt(j)) && !rightSide.contains(left_right_markers.elementAt(j)) )
				{
					remainingMarkers.add( left_right_markers.elementAt(j) );
				}
			}
			float firstLength = leftSide.firstElement().distance( rightSide.firstElement() );
			float lastLength = leftSide.lastElement().distance( rightSide.lastElement() );
			if ( firstLength > lastLength )
			{
				Vector<Vector3f> tempL = new Vector<Vector3f>();
				Vector<Vector3f> tempR = new Vector<Vector3f>();
				for ( int j = leftSide.size() - 1; j>=0; j-- )
				{
					tempL.add(leftSide.lastElement() );
					tempR.add(rightSide.lastElement() );

					leftSide.remove( leftSide.lastElement() );
					rightSide.remove( rightSide.lastElement() );
				}
				for ( int j = 0; j < tempL.size(); j++ )
				{
					leftSide.add(tempL.elementAt(j));
					rightSide.add(tempR.elementAt(j));
				}
			}

			image.resetVOIs();
			image.registerVOI(lattice);
			lattice.setColor( new Color( 0, 0, 255) );
			lattice.getCurves().elementAt(0).update( new ColorRGBA(0,0,1,1));
			lattice.getCurves().elementAt(1).update( new ColorRGBA(0,0,1,1));
			lattice.getCurves().elementAt(0).setClosed(false);
			lattice.getCurves().elementAt(1).setClosed(false);
			for ( int j = 0; j < lattice.getCurves().elementAt(0).size(); j++ )
			{
				short id = (short) image.getVOIs().getUniqueID();
				VOI marker = new VOI(id, "pair_" + j, VOI.POLYLINE, (float)Math.random() );
				VOIContour mainAxis = new VOIContour(false); 		    		    		
				mainAxis.add( lattice.getCurves().elementAt(0).elementAt(j) );
				mainAxis.add( lattice.getCurves().elementAt(1).elementAt(j) );
				marker.getCurves().add(mainAxis);
				marker.setColor( new Color( 255, 255, 0) );
				mainAxis.update( new ColorRGBA(1,1,0,1));
				if ( j == 0 )
				{
					marker.setColor( new Color( 0, 255, 0) );
					mainAxis.update( new ColorRGBA(0,1,0,1));
				}
				image.registerVOI( marker );
			}
			String voiDir = image.getImageDirectory() + JDialogBase.makeImageName( image.getImageName(), "") + File.separator;
			File voiFileDir = new File(voiDir);
			if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
			} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {
			} else { // voiFileDir does not exist
				voiFileDir.mkdir();
			}
			voiDir = image.getImageDirectory() + JDialogBase.makeImageName( image.getImageName(), "") + File.separator +
					"lattice" + File.separator;
			voiFileDir = new File(voiDir);
			if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
				String[] list = voiFileDir.list();
				for ( int file = 0; file < list.length; file++ )
				{
					File latticeFile = new File( voiDir + list[file] );
					latticeFile.delete();
				}
			} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {
			} else {
				voiFileDir.mkdir();
			}
			saveAllVOIsTo( voiDir, image );    
			
			
			
			
//			System.err.println( "growLattice " + leftSide.size() + "  " + cost.X );
//			finalLatticeLists.add(lattice);
//			cost.Y = finalLatticeLists.size() - 1;
//			finalLatticeCosts.add(cost);
			growLattice( finalLatticeLists, finalLatticeCosts, lattice, remainingMarkers );
		}
		

		latticeCount = 0;
		for ( int i = 0; i < finalLatticeLists.size(); i++ )
		{
			Vector2d cost = finalLatticeCosts.elementAt(i);
			if ( cost.X < Float.MAX_VALUE )
			{
				latticeCount++;
			}
		}

		latticeLists.clear();
		latticeCosts.clear();
		costResults = null;
		
		costResults = new Vector2d[latticeCount];
		latticeCount = 0;
		for ( int i = 0; i < costResults.length; i++ )
		{
			Vector2d cost = finalLatticeCosts.elementAt(i);
			if ( cost.X < Float.MAX_VALUE )
			{
				costResults[latticeCount++] = cost;
			}
		}
		Arrays.sort(costResults);
		for ( int i = 0; i < Math.min( costResults.length, 10 ); i++ )
		{
			Vector2d cost = costResults[i];
			VOI lattice = finalLatticeLists.elementAt( (int)cost.Y );
//			System.err.println( "  Final Lattice " + lattice.getCurves().elementAt(0).size() + " " + cost.X );
//    		costFunction( lattice.getCurves().elementAt(0), lattice.getCurves().elementAt(1), false, true );
			
			image.resetVOIs();
			image.registerVOI(lattice);
			lattice.setColor( new Color( 0, 0, 255) );
			lattice.getCurves().elementAt(0).update( new ColorRGBA(0,0,1,1));
			lattice.getCurves().elementAt(1).update( new ColorRGBA(0,0,1,1));
			lattice.getCurves().elementAt(0).setClosed(false);
			lattice.getCurves().elementAt(1).setClosed(false);
			for ( int j = 0; j < lattice.getCurves().elementAt(0).size(); j++ )
			{
				short id = (short) image.getVOIs().getUniqueID();
				VOI marker = new VOI(id, "pair_" + j, VOI.POLYLINE, (float)Math.random() );
				VOIContour mainAxis = new VOIContour(false); 		    		    		
				mainAxis.add( lattice.getCurves().elementAt(0).elementAt(j) );
				mainAxis.add( lattice.getCurves().elementAt(1).elementAt(j) );
				marker.getCurves().add(mainAxis);
				marker.setColor( new Color( 255, 255, 0) );
				mainAxis.update( new ColorRGBA(1,1,0,1));
				if ( j == 0 )
				{
					marker.setColor( new Color( 0, 255, 0) );
					mainAxis.update( new ColorRGBA(0,1,0,1));
				}
				image.registerVOI( marker );
			}
			String voiDir = image.getImageDirectory() + JDialogBase.makeImageName( image.getImageName(), "") + File.separator;
			File voiFileDir = new File(voiDir);
			if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
			} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {
			} else { // voiFileDir does not exist
				voiFileDir.mkdir();
			}
			voiDir = image.getImageDirectory() + JDialogBase.makeImageName( image.getImageName(), "") + File.separator +
					"lattice_" + i + File.separator;
			voiFileDir = new File(voiDir);
			if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
				String[] list = voiFileDir.list();
				for ( int file = 0; file < list.length; file++ )
				{
					File latticeFile = new File( voiDir + list[file] );
					latticeFile.delete();
				}
			} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {
			} else {
				voiFileDir.mkdir();
			}
			saveAllVOIsTo( voiDir, image );    
//			if ( i == 0 )
//			{
//				voiDir = image.getImageDirectory() + JDialogBase.makeImageName( image.getImageName(), "") + File.separator +
//						"lattice" + File.separator;
//				voiFileDir = new File(voiDir);
//				if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
//					String[] list = voiFileDir.list();
//					for ( int file = 0; file < list.length; file++ )
//					{
//						File latticeFile = new File( voiDir + list[file] );
//						latticeFile.delete();
//					}
//				} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {
//				} else {
//					voiFileDir.mkdir();
//				}
//				saveAllVOIsTo( voiDir, image );    
//			}    			
		}
		finalLatticeLists.clear();
		finalLatticeCosts.clear();
		costResults = null;
    }
		
		
	private boolean isBox( Vector3f pair1Left, Vector3f pair1Right, Vector3f pair2Left, Vector3f pair2Right )
    {
    	if ( pair1Left.isEqual(pair2Left) || pair1Right.isEqual(pair2Right) )
    	{
    		return false;
    	}    	
    	if ( pair1Left.isEqual(pair2Right) || pair1Right.isEqual(pair2Left) )
    	{
    		return false;
    	}    	
    	Vector3f cross1 = Vector3f.sub( pair1Right, pair1Left );  cross1.normalize();
    	Vector3f cross2 = Vector3f.sub( pair2Right, pair2Left );  cross2.normalize();
    	
    	float angle = cross1.angle(cross2);
    	if ( angle > minAngle45 )
    	{
    		return false;
    	}

    	Vector3f leftSide = Vector3f.sub( pair1Left, pair2Left );
    	Vector3f rightSide = Vector3f.sub( pair1Right, pair2Right );

    	float leftDist = leftSide.normalize();
    	float rightDist = rightSide.normalize();
    	if ( (leftDist < midLineClusterDistance) || (rightDist < midLineClusterDistance) )
    	{
    		return false;
    	}
    	angle = leftSide.angle(rightSide);
    	if ( angle > minAngle45 )
    	{
    		return false;
    	}
    	
    	
    	return true;
    }
    private boolean isEndPoint( VOIContour directions )
    {
    	float maxAngle = -Float.MAX_VALUE;
    	for ( int i = 0; i < directions.size(); i++ )
    	{
    		for ( int j = 0; j < directions.size(); j++ )
    		{
    			if ( i != j )
    			{
    				float angle = directions.elementAt(i).angle(directions.elementAt(j) );
    				if ( angle > maxAngle )
    				{
    					maxAngle = angle;
    				}
    			}
    		}
    	}
//    	System.err.println( " isEndPoint " + maxAngle );
    	if ( maxAngle > Math.PI/2.0 )
    	{
    		return false;
    	}
    	return true;
    }
	
    

    //	VOIContour headEndPoints;
//	VOIContour headInflectionPoints;
    private Vector<VOIContour> makeClusters( ModelImage image, float markerVal, Vector<Vector3f> cubeCenters, Vector<Vector3f> averages, 
    		Vector<VOIContour> lines, Vector<VOIContour> endPoints, Vector<VOIContour> inflectionPoints, 
    		int diameter, int maxDiameter )
    {
    	int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
    	int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
    	int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;    
    	   	
    	
    	
    	
    	
    	
    	Vector<VOIContour> tempLines = new Vector<VOIContour>();
    	Vector<VOIContour> clusterList = new Vector<VOIContour>();
    	for ( int i = 0; i < cubeCenters.size(); i++ )
    	{
    		Vector3f p1 = cubeCenters.elementAt(i);
    		if ( clusterList.size() == 0 )
    		{
    			VOIContour cluster = new VOIContour( false );
    			cluster.add( new Vector3f(p1) );
    			clusterList.add( cluster );   			
    		}
    		else
    		{
    			boolean added = false;
    			for ( int j = 0; j < clusterList.size(); j++ )
    			{
    				VOIContour cluster = clusterList.elementAt(j);
    				for ( int k = 0; k < cluster.size(); k++ )
    				{
    					float distance = p1.distance( cluster.elementAt(k) );
    					if ( distance < diameter )
    					{
    						cluster.add( new Vector3f( p1 ) );
    						added = true;
    						break;
    					}
    				}
    			}
    			if ( !added )
    			{
    				VOIContour cluster = new VOIContour( false );
        			cluster.add( new Vector3f(p1) );	
        			clusterList.add( cluster );
    			}
    		}
    	}	
    	
    	// Identify potential stretches of the tubes:
    	for ( int i = 0; i < clusterList.size(); i++ )
    	{
    		VOIContour cluster = clusterList.elementAt(i);
    		tempLines.add( new VOIContour(cluster) );
    	}

//    	System.err.println( tempLines.size() );
    	int[][] mergeList = new int[tempLines.size()][tempLines.size()];
    	for ( int i = 0; i < tempLines.size(); i++ )
    	{
        	for ( int j = 0; j < tempLines.size(); j++ )
        	{
        		mergeList[i][j] = -1;        		
        	}
    	}
    	for ( int i = 0; i < tempLines.size(); i++ )
    	{
    		mergeList[i][i] = i;
    		for ( int k = i+1; k < tempLines.size(); k++ )
    		{
    			if ( sameCluster( tempLines.elementAt(i), tempLines.elementAt(k) ) )
    			{
    				mergeList[i][k] = k;
    				mergeList[k][i] = i;
    			}
    			else if ( nearLine( tempLines.elementAt(i), tempLines.elementAt(k), midLineClusterDistance) )
    			{
    				mergeList[i][k] = k;
    				mergeList[k][i] = i;
    			}
    		}
    	}
    	for ( int i = 0; i < tempLines.size(); i++ )
    	{
        	for ( int j = 0; j < tempLines.size(); j++ )
        	{
        		if ( i != j )
        		{
        			mergeLines( mergeList, i, j );
        		}
        	}
    	}
    	
    	Vector<Integer> counted = new Vector<Integer>();
    	for ( int i = 0; i < tempLines.size(); i++ )
    	{
    		VOIContour contour = new VOIContour(false);
        	for ( int j = 0; j < tempLines.size(); j++ )
        	{
        		if ( mergeList[i][j] != -1 && !counted.contains(mergeList[i][j]) )
        		{  
            		counted.add(mergeList[i][j]);
            		contour.addAll(tempLines.elementAt( mergeList[i][j]));
        		}
        	}
        	if ( contour.size() > 0 )
        	{
        		lines.add( contour );
        	}
        	if ( counted.size() == tempLines.size() )
        	{
        		break;
        	}
    	}

    	if ( lines.size() <= 1 )
    	{
    		return clusterList;
    	}
    	    	
//    	System.err.println( lines.size() );
    	
    	
    	
    	float[] extents = new float[lines.size()];
    	float maxE = -1;
    	for ( int i = 0; i < lines.size(); i++ )
    	{
    		Vector3f min = new Vector3f( Float.MAX_VALUE, Float.MAX_VALUE, Float.MAX_VALUE );
    		Vector3f max = new Vector3f( -Float.MAX_VALUE, -Float.MAX_VALUE, -Float.MAX_VALUE );
			VOIContour cluster = lines.elementAt(i);
			for ( int j = 0; j < cluster.size(); j++ )
			{
				min.min( cluster.elementAt(j) );
				max.max( cluster.elementAt(j) );
			}
    		extents[i] = min.distance(max);
    	}
    	int maxIndex1 = -1;
    	maxE = -1;
    	for ( int i = 0; i < extents.length; i++ )
    	{
    		if ( extents[i] > maxE )
    		{
    			maxE = extents[i];
    			maxIndex1 = i;
    		}
    	}
    	
    	int maxIndex2 = -1;
    	maxE = -1;
    	for ( int i = 0; i < extents.length; i++ )
    	{
    		if ( (i != maxIndex1) && (extents[i] > maxE) )
    		{
    			maxE = extents[i];
    			maxIndex2 = i;
    		}
    	}

    	VOIContour max1 = lines.elementAt(maxIndex1);
    	VOIContour max2 = null;
    	if ( maxIndex2 != -1 )
    	{
    		max2 = lines.elementAt(maxIndex2);
    	}
    	lines.remove(max1);
    	lines.add(0,max1);
    	
    	if ( max2 != null )
    	{
    		lines.remove(max2);
    		lines.add(1,max2);
    	}

    	head = lines.elementAt(0);
    	for ( int i = 1; i < lines.size(); i++ )
    	{
    		head.addAll(lines.elementAt(i));
    	}
    	fillHead( image, max_head );
    	head.clear();

    	for ( int i = 0; i < lines.size(); i++ )
    	{
//    		System.err.print( "Lines " + i + " " + lines.elementAt(i).size() );    	
    		averageContour(lines.elementAt(i) );
//    		System.err.println( "   ==>   " + i + " " + lines.elementAt(i).size() );    		
    	}

    	for ( int i = lines.size() - 1; i >= 0; i-- )
    	{
    		if ( lines.elementAt(i).size() < 2 )
    		{
    			lines.remove(i);
    		}
    	}

    	for ( int c = 0; c < lines.size(); c++ )
    	{
    		VOIContour contour = lines.elementAt(c);
//    		System.err.println( contour.size() );
    		for ( int i = contour.size() - 1; i >=0; i-- )
    		{
    			for ( int j = i-1; j >=0; j-- )
    			{
    				if ( contour.elementAt(i).isEqual( contour.elementAt(j) ) )
    				{
    					contour.remove(i);
    					break;
    				}
    			}
    		}
//    		System.err.println( contour.size() );

    		Vector3f min = new Vector3f(dimX,dimY,dimZ);
    		Vector3f max = new Vector3f(-1,-1,-1);
    		for ( int i = 0; i < contour.size(); i++ )
    		{
    			min.min(contour.elementAt(i));
    			max.max(contour.elementAt(i));
    		}
    		VOIContour headEndPoints = new VOIContour(false);
    		for ( int i = 0; i < contour.size(); i++ )
    		{
    			Vector3f pt = contour.elementAt(i);
    			if ( (pt.X == min.X) || (pt.Y == min.Y) || (pt.Z == min.Z) ||
    					(pt.X == max.X) || (pt.Y == max.Y) || (pt.Z == max.Z) )
    			{
    				headEndPoints.add( new Vector3f(contour.elementAt(i)) );
    			}
    		}
    		VOIContour[] directions = new VOIContour[contour.size()];
    		for ( int i = 0; i < contour.size(); i++ )
    		{
    			float minDist = Float.MAX_VALUE;
    			for ( int j = 0; j < contour.size(); j++ )
    			{
    				if ( !contour.elementAt(i).isEqual( contour.elementAt(j) ) )
    				{
    					float distance = contour.elementAt(i).distance( contour.elementAt(j) );
    					if ( distance < minDist )
    					{
    						minDist = distance;
    					}
    				}
    			}
    			minDist += 10;
    			directions[i] = new VOIContour(false);
    			for ( int j = 0; j < contour.size(); j++ )
    			{
    				if ( !contour.elementAt(i).isEqual( contour.elementAt(j) ) )
    				{
    					float distance = contour.elementAt(i).distance( contour.elementAt(j) );
    					if ( distance < minDist )
    					{
    						Vector3f dir = Vector3f.sub( contour.elementAt(i), contour.elementAt(j) );
    						dir.normalize();
    						directions[i].add( dir );
    					}
    				}
    			}
    		}
    		for ( int i = 0; i < directions.length; i++ )
    		{
    			if ( isEndPoint( directions[i] ) )
    			{
    				headEndPoints.add( new Vector3f(contour.elementAt(i)) );
    			}
    		}

/*
    		Vector<Vector3f> tempHeadEndPoints = new Vector<Vector3f>();
    		for ( int i = 0; i < headEndPoints.size(); i++ )
    		{
    			for ( int j = i+1; j < headEndPoints.size(); j++ )
    			{
    				System.err.println( headEndPoints.elementAt(i).distance( headEndPoints.elementAt(j) ) );
    				if ( headEndPoints.elementAt(i).distance( headEndPoints.elementAt(j) ) < midLineClusterDistance )
    				{
    					Vector3f temp = Vector3f.add( headEndPoints.elementAt(i), headEndPoints.elementAt(j) );
    					temp.scale(0.5f);
    					tempHeadEndPoints.add(temp);
    				}
    			}
    		}
    		for ( int i = headEndPoints.size() -1; i >= 0; i-- )
    		{
    			for ( int j = 0; j < tempHeadEndPoints.size(); j++ )
    			{
    				if ( headEndPoints.elementAt(i).distance( tempHeadEndPoints.elementAt(j) ) < midLineClusterDistance )
    				{
    					headEndPoints.remove(i);
    					break;
    				}
    			}
    		}
    		headEndPoints.addAll(tempHeadEndPoints);
*/

//    		System.err.println( headEndPoints.size() );
//
//    		float minDist = Float.MAX_VALUE;
//    		for ( int i = 0; i < headEndPoints.size(); i++ )
//    		{
//    			for ( int j = 0; j < contour.size(); j++ )
//    			{
//    				if ( !headEndPoints.elementAt(i).isEqual( contour.elementAt(j) ) )
//    				{
//    					float distance = headEndPoints.elementAt(i).distance( contour.elementAt(j) );
//    					if ( distance < minDist )
//    					{
//    						minDist = distance;
//    					}
//    				}
//    			}
//    		}
//    		System.err.println( minDist );
    		
    		endPoints.add(headEndPoints);
    	}

    	for ( int c = 0; c < lines.size(); c++ )
    	{
    		VOIContour contour = lines.elementAt(c); 
    		VOIContour headEndPoints = endPoints.elementAt(c);
    		for ( int i = 0; i < headEndPoints.size(); i++ )
    		{
    			if ( !contour.contains(headEndPoints.elementAt(i) ) )
    			{
    				contour.add(headEndPoints.elementAt(i) );
    			}
    		}
    		
    		VOIContour headInflectionPoints = new VOIContour(false);  
        	averageContour(headEndPoints);
        	if ( headEndPoints.size() > 2 )
        	{
        		float[] minDistances = new float[headEndPoints.size()];
        		for ( int i = 0; i < headEndPoints.size(); i++ )
        		{
        			minDistances[i] = Float.MAX_VALUE;
        			for ( int j = 0; j < contour.size(); j++ )
        			{
        				if ( !headEndPoints.elementAt(i).isEqual( contour.elementAt(j) ) )
        				{
        					float distance = headEndPoints.elementAt(i).distance( contour.elementAt(j) );
        					if ( distance < minDistances[i] )
        					{
        						minDistances[i] = distance;
        					}
        				}
        			}
        		}
        		Arrays.sort(minDistances);
        		float maxSpan = minDistances[minDistances.length-1];
        		if ( maxSpan == Float.MAX_VALUE )
        		{
        			for ( int i = minDistances.length-1; i >= 0; i-- )
        			{
        				if ( minDistances[i] < Float.MAX_VALUE )
        				{
        					maxSpan = minDistances[i];
        					break;
        				}
        			}
        		}
        		maxSpan += 10;
//        		System.err.println( "maxSpan = " + maxSpan );

        		Vector2d[] minAngles = new Vector2d[headEndPoints.size()];
        		for ( int i = 0; i < headEndPoints.size(); i++ )
        		{
        			VOIContour direction = new VOIContour(false);
        			for ( int j = 0; j < contour.size(); j++ )
        			{
        				if ( !headEndPoints.elementAt(i).isEqual( contour.elementAt(j) ) )
        				{
        					float distance = headEndPoints.elementAt(i).distance( contour.elementAt(j) );
        					if ( distance <= maxSpan )
        					{
        						Vector3f dir = Vector3f.sub( headEndPoints.elementAt(i), contour.elementAt(j) );
        						dir.normalize();
        						direction.add( dir );
        					}
        				}
        			}
        			minAngles[i] = new Vector2d();
        			minAngles[i].X = maxAngle( direction );
        			minAngles[i].Y = i;
        		}

        		Arrays.sort(minAngles);
        		Vector3f end1 = headEndPoints.elementAt( (int)minAngles[0].Y  );
        		Vector3f end2 = headEndPoints.elementAt( (int)minAngles[1].Y  );
        		int end2Index = 1;
        		for ( int i = 2; i < minAngles.length; i++ )
        		{
        			if ( end1.distance(end2) > midLineMarkerDistance )
        			{
        				break;
        			}
        			end2 = headEndPoints.elementAt( (int)minAngles[i].Y  );
        			end2Index = i;
        		}
//        		System.err.println( minAngles[0].X + " " + minAngles[0].Y );
//        		System.err.println( minAngles[end2Index].X + " " + minAngles[end2Index].Y );
        		Vector3f temp1 = new Vector3f(headEndPoints.elementAt( (int)minAngles[0].Y ));
        		Vector3f temp2 = new Vector3f(headEndPoints.elementAt( (int)minAngles[end2Index].Y ));

        		headEndPoints.remove(temp1);
        		headEndPoints.remove(temp2);
        		headInflectionPoints.addAll(headEndPoints);
        		headEndPoints.clear();
        		headEndPoints.add(temp1);
        		headEndPoints.add(temp2);
        	}

    		inflectionPoints.add(headInflectionPoints);
    	}

    	return clusterList;    	
    }
	

    
    private Vector<VOIContour> makeClusters( Vector<Vector3f> cubeCenters, int diameter, int maxDiameter )
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
    private float maxAngle( VOIContour directions )
    {
    	float maxAngle = -Float.MAX_VALUE;
    	for ( int i = 0; i < directions.size(); i++ )
    	{
    		for ( int j = 0; j < directions.size(); j++ )
    		{
    			if ( i != j )
    			{
    				float angle = directions.elementAt(i).angle(directions.elementAt(j) );
    				if ( angle > maxAngle )
    				{
    					maxAngle = angle;
    				}
    			}
    		}
    	}
    	return maxAngle;
    }
    
    

    private void mergeLines( int[][] temp, int index1, int index2 )
    {
    	boolean merge = false;
    	for ( int i = 0; i < temp[index1].length; i++ )
    	{
    		if ( (temp[index1][i] == i) && (temp[index2][i] == i) )
    		{
    			merge = true;
    		}
    	}
    	if ( merge )
    	{
        	for ( int i = 0; i < temp[index1].length; i++ )
        	{
        		if ( (temp[index1][i] == i) || (temp[index2][i] == i) )
        		{
        			temp[index1][i] = i;
        			temp[index2][i] = i;
        		}
        	}    		
    	}
    }
    
    private boolean mergePoints( ModelImage image, float intensityMin, Vector3f p1, Vector3f p2 )
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
    
    

    
    private Vector3f[] nearest2Pts( Vector3f pt, Vector<Vector3f> list )
    {
    	Vector3f[] pair = new Vector3f[]{null,null};
		float minFound = Float.MAX_VALUE;
		int minIndex = -1;
    	for ( int i = 0; i < list.size(); i++ )
    	{
			float d = pt.distance( list.elementAt(i) );
    		if ( d < minFound )
    		{
    			minFound = d;
    			minIndex = i;
    			pair[0] = list.elementAt(i);
    		}
    	}
		minFound = Float.MAX_VALUE;
    	for ( int i = 0; i < list.size(); i++ )
    	{
    		if ( i == minIndex )
    			continue;
			float d = pt.distance( list.elementAt(i) );
    		if ( d < minFound )
    		{
    			minFound = d;
    			pair[1] = list.elementAt(i);
    		}
    	}
    	if ( pair[1] == null )
    	{
    		pair[1] = pair[0];
    	}
    	return pair;
    }
    
    
    private boolean nearLine( VOIContour line1, VOIContour line2, int distance )
    {
    	for ( int i = 0; i < line1.size(); i++ )
    	{
    		for ( int j = 0; j < line2.size(); j++ )
    		{
    			if ( line1.elementAt(i).distance(line2.elementAt(j)) < distance )
    			{
    				return true;
    			}
    		}
    	}
    	return false;
    }
	
    
    
    /**
     * This function determines the shortest path from the start vertex through the input vertex to the vertices that
     * neighbor the input vertex.
     *
     * <p>The function relaxEdges looks at all the vertices connected by triangle edges to the input vertex and
     * calculated the weight factors for each of those vertices, adding those vertices that are not yet "relaxed" to the
     * list of vertices on the border.</p>
     *
     * <p>Once the weight factors for each of the vertices connected to the input vertex are set, the input vertex is
     * labeled "relaxed" and removed from the list of vertices on the border.</p>
     *
     * @param  iNode  int input vertex index
     */
    private void relaxEdges( ModelImage image, Vector3f seed, int iNode) {
        int iPreviousSave;
        float fPathLength;

    	int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
    	int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
    	int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;    
        

		for ( int z = (int) (seed.Z-1); z <= seed.Z+1; z++ )
		{
			for ( int y = (int) (seed.Y-1); y <= seed.Y+1; y++ )
			{
				for ( int x = (int) (seed.X-1); x <= seed.X+1; x++ )
				{
					if ( (x >= 0) && (x < dimX) && (y >= 0) && (y < dimY) && (z >= 0) && (z < dimZ) )
					{
						if ( x != seed.X || y != seed.Y || z != seed.Z )
						{
							int iNeighbor = (z * dimX * dimY + y * dimX + x);
							if ( image.getMask().get(iNeighbor) )
							{
								if ( !relaxed[iNeighbor] )
								{

						            /* Calculated the edge distance between the neighbor vertex and
						             * the input vertex. This is done by looking at the smoothed path through the current node to the neighbor.
						             * If the neighbor already has a path through it, then save it incase it's
						             * smaller: */
						            iPreviousSave = previous[iNeighbor];
						            previous[iNeighbor] = iNode;

						            Vector3f newSeed = new Vector3f(x,y,z);
						            fPathLength = weight[iNode] + seed.distance( newSeed );


						            /* If this is the first time iNeighbor has been touched, or if the
						             * path is shorter passing through the input node, then update the
						             * wieght: */
						            if ((iPreviousSave == -1) || (weight[iNeighbor] > fPathLength)) {
						                weight[iNeighbor] = fPathLength;

						                /* If the neighbor vertex has not yet been relaxed, add it to
						                 * the list of vertices on the border -- so that it can be
						                 * relaxed */
						                if (!relaxed[iNeighbor]) {
						                    if (!border.contains(newSeed)) {
						                        border.add(newSeed);
						                    }
						                }
						            }
						            /* Otherwise, revert to the original path for iNeighbor: */
						            else {
						            	previous[iNeighbor] = iPreviousSave;
						            }
								}
							}
						}
					}
				}
			}
		}

        /* Set the relaxed flag for this vertex to true. We now know the
         * shortest path from the start vertex to this vertex. */
        relaxed[iNode] = true;

        /* Remove this vertex from the list of vertices that can be
         * relaxed. */
        if (border.contains(seed)) {
            border.remove(seed);
        }
    }
    
    
    private ModelImage robustHistogram(ModelImage image, Vector<Vector3f> dataPoints )
    {
//    	ModelImage result = (ModelImage)image.clone();
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
    	int count_995P = (int) (0.995 * totalCount);
    	int count_999P = (int) (0.999 * totalCount);
    	int count_99P = (int) (0.990 * totalCount);
    	
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
    	boolean maxHeadFound = false;
    	boolean maxLRFound = false;
    	for ( int i = 0; i < counts.length; i++ )
    	{
    		counts[i] = map.get( keyArray[i] );
    		runningCount += counts[i];

    		if ( !minFound && (runningCount >= count_2P) )
    		{
    			min_2P = keyArray[i];
    			minFound = true;
    		}
    		if ( !maxHeadFound && (runningCount >= count_99P) )
    		{
    			max_head = keyArray[i];
    			maxHeadFound = true;
    		}
    		if ( !maxLRFound && (runningCount >= count_999P) )
    		{
    			max_LR = keyArray[i];
    			maxLRFound = true;
    		}
    	}
    	
    	max_head = max_LR;
    	
    	Preferences.debug( "Image min = " + min + " max = " + max + "\n", Preferences.DEBUG_ALGORITHM );
    	Preferences.debug( "Robust min = " + min_2P + " robust max = " + max_LR + "\n", Preferences.DEBUG_ALGORITHM );

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
//    				result.set(x, y, z, value );
    				if ( value >= (.90f*max_LR) )
    				{
    					dataPoints.add( new Vector3f( x, y, z ) );
    				}
    			}
    		}
    	}
//    	new ViewJFrameImage( result );
//    	return result;
    	return null;
    }	
    
    private boolean sameCluster( VOIContour line1, VOIContour line2 )
    {
    	for ( int i = 0; i < line1.size(); i++ )
    	{
    		for ( int j = 0; j < line2.size(); j++ )
    		{
    			if ( line1.elementAt(i).equals(line2.elementAt(j)) )
    			{
    				return true;
    			}
    		}
    	}
    	return false;
    }
    
    /**
     * This method saves all VOIs for the active image to a given directory.
     * @param voiDir directory that contains VOIs for this image.
     */
    private void saveAllVOIsTo(final String voiDir, ModelImage image) {
        try {
            ViewVOIVector VOIs = image.getVOIs();

            final File voiFileDir = new File(voiDir);

            if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
            } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
            } else { // voiFileDir does not exist
                voiFileDir.mkdir();
            }

            int nVOI = VOIs.size();

            for (int i = 0; i < nVOI; i++) {
                if (VOIs.VOIAt(i).getCurveType() != VOI.ANNOTATION) {
                	FileVOI fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + ".xml", voiDir, image);
                	fileVOI.writeXML(VOIs.VOIAt(i), true, true);
                }
                else {
                    FileVOI fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + ".lbl", voiDir, image);
                    fileVOI.writeAnnotationInVoiAsXML(VOIs.VOIAt(i).getName(),true);             	
                }
            }

        } catch (final IOException error) {
            MipavUtil.displayError("Error writing all VOIs to " + voiDir + ": " + error);
        }

    } // end saveAllVOIsTo()
    
    private void saveLeftRightMarkers( ModelImage image )
    {
    	if ( left_right_markers == null )
    	{
    		return;
    	}
    	image.resetVOIs();
//    	short id = (short) image.getVOIs().getUniqueID();
//    	for ( int i = 0; i < left_right_markers.size(); i++ )
//    	{
//    		Vector3f pt = left_right_markers.elementAt(i);    	        
//    		VOI marker = new VOI(id++, "marker_" + i, VOI.POINT, (float)Math.random() );
//    		marker.importPoint(pt);
//    		marker.setColor( Color.yellow );
//    		marker.getCurves().elementAt(0).update( new ColorRGBA(1, 1, 0, 1));
//    		image.registerVOI(marker);
//    	}
//		String voiDir = image.getImageDirectory() + JDialogBase.makeImageName( image.getImageName(), "") + File.separator;
//        File voiFileDir = new File(voiDir);
//        if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
//        } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
//        } else { // voiFileDir does not exist
//            voiFileDir.mkdir();
//        }
//		voiDir = image.getImageDirectory() + JDialogBase.makeImageName( image.getImageName(), "") + File.separator +
//    			"left_right_markers" + File.separator;
//        voiFileDir = new File(voiDir);
//        if (voiFileDir.exists() && voiFileDir.isDirectory()) {
//        	String[] list = voiFileDir.list();
//        	for ( int i = 0; i < list.length; i++ )
//        	{
////        		System.err.println( list[i] );
//        		File lrFile = new File( voiDir + list[i] );
//        		lrFile.delete();
//        	}
//        } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {
//        } else { // voiFileDir does not exist
//            voiFileDir.mkdir();
//        }
//    	saveAllVOIsTo( voiDir, image );
    	
    	for ( int i = 0; i < left_right_markers.size(); i++ )
    	{
    		Vector3f pt = left_right_markers.elementAt(i);   
    		int colorID = 0;
    		VOI newTextVOI = new VOI((short) colorID, "annotation3d_" + i, VOI.ANNOTATION, -1.0f);
    		VOIText textVOI = new VOIText( );
    		textVOI.add( pt );
    		textVOI.add( Vector3f.add( new Vector3f(2,0,0), pt) );
    		textVOI.setText("LR_"+i);
    		newTextVOI.getCurves().add(textVOI);
    		image.registerVOI(newTextVOI);
    	}
		String voiDir = image.getImageDirectory() + JDialogBase.makeImageName( image.getImageName(), "") + File.separator;
        File voiFileDir = new File(voiDir);
        if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
        } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
        } else { // voiFileDir does not exist
            voiFileDir.mkdir();
        }
		voiDir = image.getImageDirectory() + JDialogBase.makeImageName( image.getImageName(), "") + File.separator +
    			"annotated_left_right_markers" + File.separator;
        voiFileDir = new File(voiDir);
        if (voiFileDir.exists() && voiFileDir.isDirectory()) {
        	String[] list = voiFileDir.list();
        	for ( int i = 0; i < list.length; i++ )
        	{
//        		System.err.println( list[i] );
        		File lrFile = new File( voiDir + list[i] );
        		lrFile.delete();
        	}
        } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {
        } else { // voiFileDir does not exist
            voiFileDir.mkdir();
        }
    	saveAllVOIsTo( voiDir, image );
    }


    private void saveMaxIntensityProjection( ModelImage image, BitSet headMask, BitSet leftRightMask )
    {

    	int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
    	int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
    	int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
    	ModelImage output2D = new ModelImage( ModelStorageBase.ARGB, new int[]{dimX,dimY}, JDialogBase.makeImageName( image.getImageName(), "_mp.tif") );
    	image.calcMinMax();
    	float min = (float) image.getMin();
    	float max = (float) image.getMax();
    	for ( int y = 0; y < dimY; y++ )
    	{
    		for ( int x = 0; x < dimX; x++ )
    		{
    			float maxV = -Float.MAX_VALUE;
    			boolean leftRightOn = false;
    			boolean headOn = false;
    			for ( int z = 0; z < dimZ; z++ )
    			{
    				float value = image.getFloat(x,y,z);
    				if ( value > maxV )
    				{
    					maxV = value;
    				}
					int index = (z * dimX * dimY + y * dimX + x);
					if ( leftRightMask.get(index) )
					{
						leftRightOn = true;
					}
					if ( headMask.get(index) )
					{
						headOn = true;
					}
    			}
    			float scaledValue = 255*(maxV - min)/(max - min);
    			output2D.setC(x, y, 0, 1);
    			output2D.setC(x, y, 1, scaledValue);
    			output2D.setC(x, y, 2, scaledValue);
    			output2D.setC(x, y, 3, scaledValue);
    			if ( leftRightOn && saveLeftRightMarkers )
    			{
        			output2D.setC(x, y, 1, scaledValue);
        			output2D.setC(x, y, 2, 0);
        			output2D.setC(x, y, 3, 0);
    			}
    			if ( headOn && saveMesh )
    			{
        			output2D.setC(x, y, 1, 0);
        			output2D.setC(x, y, 2, scaledValue);
        			output2D.setC(x, y, 3, scaledValue);
    			}
    		}
    	}
		String aviDir = image.getImageDirectory() + "maximum_intensity_projections" + File.separator;
        File aviFileDir = new File(aviDir);
        if (aviFileDir.exists() && aviFileDir.isDirectory()) { // do nothing
        } else if (aviFileDir.exists() && !aviFileDir.isDirectory()) { // voiFileDir.delete();
        } else { // voiFileDir does not exist
        	aviFileDir.mkdir();
        }
    	output2D.saveImage(aviDir, output2D.getImageName(), FileUtility.TIFF, false);
    	output2D.disposeLocal();
    }

    
    private void saveMesh( ModelImage image, TriMesh mesh, final boolean flip, final int count) 
	{
    	int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
    	int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
    	int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;   
    	
        TransMatrix dicomMatrix = null;
        TransMatrix inverseDicomMatrix = null;
        // double[][] inverseDicomArray = null;
        float[] coord;
        float[] tCoord;

		int iVQuantity = mesh.VBuffer.GetVertexQuantity();

    	float[] res = image.getResolutions(0);
        float[] startLocation = image.getFileInfo()[0].getOrigin();
        int[] direction = MipavCoordinateSystems.getModelDirections(image);
        
        Vector3f[] transformedPositions = new Vector3f[iVQuantity];
        if ( image.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL) )
        {

            // Get the DICOM transform that describes the transformation from
            // axial to this image orientation
            dicomMatrix = image.getMatrix();
            inverseDicomMatrix = new TransMatrix(image.getMatrix());
            inverseDicomMatrix.Inverse();
            // inverseDicomArray = inverseDicomMatrix.getMatrix();
            // inverseDicomMatrix = null;
            coord = new float[3];
            tCoord = new float[3];

            for ( int i = 0; i < iVQuantity; i++)
            {
            	Vector3f pos = mesh.VBuffer.GetPosition3(i);
            	
                // Change the voxel coordinate into millimeter space
                coord[0] = pos.X * res[0];
                coord[1] = pos.Y * res[1];
                coord[2] = pos.Z * res[2];

                // Convert the point to axial millimeter DICOM space
                dicomMatrix.transform(coord, tCoord);

                // Add in the DICOM origin
                pos.X = startLocation[0] + tCoord[0];
                pos.Y = startLocation[1] + tCoord[1];
                pos.Z = startLocation[2] + tCoord[2];
                transformedPositions[i] = pos;
            }
        }
        else
        {
            for ( int i = 0; i < iVQuantity; i++ )
            {
            	Vector3f pos = mesh.VBuffer.GetPosition3(i);
            	pos.X = (pos.X * res[0] * direction[0]) + startLocation[0];
            	pos.Y = (pos.Y * res[1] * direction[1]) + startLocation[1];
            	pos.Z = (pos.Z * res[2] * direction[2]) + startLocation[2];
            	transformedPositions[i] = pos;
            }
        }


        float[] box = new float[3];
        box[0] = (dimX - 1) * res[0];
        box[1] = (dimY - 1) * res[1];
        box[2] = (dimZ - 1) * res[2];
        
        

		String voiDir = image.getImageDirectory() + JDialogBase.makeImageName( image.getImageName(), "") + File.separator;
        File voiFileDir = new File(voiDir);
        if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
        } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
        } else { // voiFileDir does not exist
            voiFileDir.mkdir();
        }
		voiDir = image.getImageDirectory() + JDialogBase.makeImageName( image.getImageName(), "") + File.separator +
    			"head_meshes" + File.separator;
        voiFileDir = new File(voiDir);
        if (voiFileDir.exists() && voiFileDir.isDirectory()) {
        	String[] list = voiFileDir.list();
        	for ( int i = 0; i < list.length; i++ )
        	{
//        		System.err.println( list[i] );
        		File meshFile = new File( voiDir + list[i] );
        		meshFile.delete();
        	}
        } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {
        } else { // voiFileDir does not exist
            voiFileDir.mkdir();
        }
        
        String kName = voiDir + "headSurface" + count + ".xml";
        
        TriMesh kMesh = new TriMesh( new VertexBuffer(transformedPositions), new IndexBuffer(mesh.IBuffer));
//        System.err.println( "OpenCLAlgorithmMarchingCubes " + kMesh.VBuffer.GetVertexQuantity() + " " + kMesh.GetTriangleQuantity() );
        try {
			FileSurface_WM.save(kName, kMesh, 0, kMesh.VBuffer, flip, direction, startLocation, box, inverseDicomMatrix);
		} catch (IOException e) {}
    }

    private void scaleMesh( TriMesh mesh, float fValue, HashSet[] connections )
    {
        int iVQuantity = mesh.VBuffer.GetVertexQuantity();
        VertexBuffer kVBuffer = new VertexBuffer( mesh.VBuffer );

        int num;
        Vector3f kSum = new Vector3f();
        Vector3f kOriginalPos = new Vector3f();
        Vector3f kConnectionPos = new Vector3f();

        // for each coordinate vertex
        for (int i = 0; i < iVQuantity; i++) {

            kSum.set(0f,0f,0f);
            num = 0;
            mesh.VBuffer.GetPosition3(i, kOriginalPos);

            // get all the verticies that are connected to this one (at i)
            for (Iterator iter = connections[i].iterator(); iter.hasNext();) {
                int index = ((Integer) iter.next()).intValue();

                mesh.VBuffer.GetPosition3(index, kConnectionPos);

                // Sum of (xj - xi) where j ranges over all the points connected to xi
                // xj = m_kV2; xi = m_kV3
                kConnectionPos.sub( kOriginalPos );
                kSum.add( kConnectionPos );
                num++;
            }
            // xi+1 = xi + (alpha)*(sum of(points xi is connected to - xi))

            if (num > 1) {
                kSum.scale( 1.0f / num );
            }

            kSum.scale( fValue );
            kOriginalPos.add( kSum );
            kVBuffer.SetPosition3(i, kOriginalPos);
        }

        for (int i = 0; i < iVQuantity; i++) {
        	mesh.VBuffer.SetPosition3(i, kVBuffer.GetPosition3(i) );
        }

        kVBuffer.dispose();
        kVBuffer = null;
    }

	private void swapColumn( int[][] table, int row, int column1, int column2, int n )
    {
    	for ( int i = 0; i < n; i++ )
    	{
    		int temp = table[row+i][column1];
    		table[row+i][column1] = table[row+i][column2];
    		table[row+i][column2] = temp;
    	}
    }
	
	private void findLeftRightMarkers( ModelImage image, float minValue )
	{
    	int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
    	int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
    	int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;  

    	int length = dimX * dimY * dimZ;

		Vector<BitSet> lrMasks = new Vector<BitSet>();
		Vector<VOIContour> lrPoints = new Vector<VOIContour>();
		while( (lrMasks.size() < 25) && (minValue >= max_LR) )
		{
			System.err.println( lrMasks.size() + "  " + minValue );
			BitSet visited = new BitSet(length);
			for ( int z = 0; z < dimZ; z++ )
			{
				for ( int y = 0; y < dimY; y++ )
				{
					for ( int x = 0; x < dimX; x++ )
					{
						int index = z * dimX * dimY + y * dimX + x;
						float value = image.getFloat(x, y, z);
						if ( (value >= minValue) && !visited.get(index) )
						{
							BitSet mask = new BitSet(length);
							VOIContour maskPoints = new VOIContour(false);

							Vector<Vector3f> seedList = new Vector<Vector3f>();
							Vector3f seed = new Vector3f(x,y,z);    		
							visited.set(index);
							maskPoints.add(seed);
							seedList.add(seed);

							fill( image, minValue, seedList, visited, mask, maskPoints );
							if ( mask.cardinality() > 1 )
							{								
//								Box3f box = ContBox3f.ContOrientedBox(maskPoints.size(), maskPoints);
//								if ( (box.Extent[0] > 1) && (box.Extent[1] > 1) && (box.Extent[2] > 1) )
								{
									lrMasks.add(mask);
//									lrBoxes.add( box );
									lrPoints.add(maskPoints);
								}
							}
						}
					}
				}
			}
			for ( int i = lrMasks.size() - 1; i >=0; i-- )
			{
				int intersectCount = 0;
				for ( int j = i-1; j >= 0; j-- )
				{
					if ( lrMasks.elementAt(i).intersects( lrMasks.elementAt(j) ) )
					{
						intersectCount++;
					}
				}
//				if ( intersectCount == 1 )
				{
					for ( int j = i-1; j >= 0; j-- )
					{
						if ( lrMasks.elementAt(i).intersects( lrMasks.elementAt(j) ) )
						{
							lrMasks.elementAt(j).or(lrMasks.elementAt(i) );
							lrMasks.remove(i);

							lrPoints.elementAt(j).addAll( lrPoints.elementAt(i) );
							lrPoints.remove(i);
							break;
						}
					}
				}
			}
//			if ( lrMasks.size() == 0 )
			{
				minValue *= 0.95f;
			}
		}
		
		
		BitSet finalHMask = new BitSet(length);
		BitSet finalMask = new BitSet(length);
		System.err.println( lrMasks.size() );
		image.resetVOIs();
    	short id = (short) image.getVOIs().getUniqueID();
		for ( int i = 0; i < lrMasks.size(); i++ )
		{
			VOIContour maskPoints = lrPoints.elementAt(i);
			Box3f box = ContBox3f.ContOrientedBox(maskPoints.size(), maskPoints);
			System.err.println( box.Extent[0] + "  " + box.Extent[1] + "  " + box.Extent[2] );	
			
    		VOI marker = new VOI(id++, "marker_" + i, VOI.POINT, (float)Math.random() );
    		marker.importPoint(box.Center);
    		marker.setColor( Color.yellow );
    		marker.getCurves().elementAt(0).update( new ColorRGBA(1, 1, 0, 1));
    		image.registerVOI(marker);
    		
    		if ( (box.Extent[0] < 50) && (box.Extent[1] < 50) && (box.Extent[2] < 50) )
    		{
    			finalMask.or( lrMasks.elementAt(i) );
    		}
    		else
    		{
    			finalHMask.or( lrMasks.elementAt(i) );
    		}
		}
		if ( finalHMask.cardinality() > 0 )
		{
			int index = finalHMask.nextSetBit(0);
			int x = index % dimX;
			index -= x;
			index /= dimX;
			
			int y = index % dimY;
			index -= y;
			index /= dimY;
			
			int z = index;
			
			BitSet visited = new BitSet(length);
			BitSet mask = new BitSet(length);
			VOIContour maskPoints = new VOIContour(false);

			Vector<Vector3f> seedList = new Vector<Vector3f>();
			Vector3f seed = new Vector3f(x,y,z);    		
			visited.set(index);
			maskPoints.add(seed);
			seedList.add(seed);

			fill( image, 0.90f * minValue, seedList, visited, mask, maskPoints );
			
			
			image.setMask(finalHMask);
			new ViewJFrameImage((ModelImage)image.clone());
			
		}
		
		
		image.setMask(finalMask);
		new ViewJFrameImage((ModelImage)image.clone());
	}
	
}




