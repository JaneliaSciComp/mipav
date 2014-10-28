package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.ModelStorageBase.DataType;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.ViewVOIVector;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOILatticeManagerInterface;

import java.awt.Color;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.BitSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.JFileChooser;

import WildMagic.LibFoundation.Containment.ContBox3f;
import WildMagic.LibFoundation.Curves.NaturalSpline3;
import WildMagic.LibFoundation.Distance.DistanceSegment3Segment3;
import WildMagic.LibFoundation.Distance.DistanceVector3Plane3;
import WildMagic.LibFoundation.Distance.DistanceVector3Segment3;
import WildMagic.LibFoundation.Mathematics.Box3f;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Ellipsoid3f;
import WildMagic.LibFoundation.Mathematics.Plane3f;
import WildMagic.LibFoundation.Mathematics.Segment3f;
import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector4f;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.Transformation;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

public class LatticeModel {

    private ModelImage imageA;
    private ModelImage imageB;
    private VOIVector latticeGrid;    

    private VOI lattice = null;
    private VOIContour left;
    private VOIContour right;
    private VOIContour center;
    private VOIContour leftBackup;
    private VOIContour rightBackup;
    private float[] afTimeC;
    private float[] allTimes;

    private NaturalSpline3 centerSpline;
    private NaturalSpline3 leftSpline;
    private NaturalSpline3 rightSpline;
    private VOIContour centerPositions;
    
    private VOIContour leftPositions;
    private VOIContour rightPositions;
    

	private float length;
    private VOI leftLine;
    private VOI rightLine;
	private VOI centerLine;
	
    private Vector<Float> wormDiameters;
    private Vector<Vector3f> rightVectors;
    private Vector<Vector3f> upVectors;

	private int extent = -1;
    
    private Vector<Box3f> boxBounds;
    private Vector<Ellipsoid3f> ellipseBounds;
        

	private VOI samplingPlanes;
	private VOI displayContours;    
	private VOI displayInterpolatedContours;    
	private Vector3f pickedPoint = null;
	private int pickedAnnotation = -1;
	private VOI showSelectedVOI = null;
	private VOIContour[] showSelected = null;
	private int DiameterBuffer = 0;
	private int SampleLimit = 5;
	private	float minRange = .025f;
	
	private VOI leftMarker;	
	private VOI rightMarker;
	
	private VOI growContours;
	
	private VOI annotationVOIs;
	private Vector3f wormOrigin = null;
	
	public LatticeModel( ModelImage imageA, ModelImage imageB )
	{
		this.imageA = imageA;
		this.imageB = imageB;
	}
    
    public LatticeModel( ModelImage imageA, ModelImage imageB, VOI lattice )
	{
		this.imageA = imageA;
		this.imageB = imageB;
		this.lattice = lattice;

		// Assume image is isotropic (square voxels).
		if ( lattice.getCurves().size() != 2 )
		{
			return;
		}
		left = (VOIContour) lattice.getCurves().elementAt(0);
		right = (VOIContour) lattice.getCurves().elementAt(1);
		if ( left.size() != right.size() )
		{
			return;
		}
		
		this.imageA.registerVOI(lattice);
		updateLattice(true);
	}
    
    public LatticeModel( ModelImage imageA, ModelImage imageB, VOI annotation, boolean doAnnotation )
	{
		this.imageA = imageA;
		this.imageB = imageB;
		this.lattice = null;
		this.setAnnotations(annotation);
	}
    
    public void addAnnotation( VOI textVOI )
    {
    	if ( annotationVOIs == null )
    	{
			int colorID = 0;
			annotationVOIs = new VOI((short) colorID, "annotationVOIs", VOI.ANNOTATION, -1.0f);
			imageA.registerVOI( annotationVOIs );
    	}
    	VOIText text = (VOIText) textVOI.getCurves().firstElement().clone();
    	Color c = text.getColor();
    	text.update( new ColorRGBA( c.getRed()/255.0f, c.getGreen()/255.0f, c.getBlue()/255.0f, 1f ));
    	annotationVOIs.getCurves().add( text );  
    	annotationVOIs.setColor(c);

		if ( text.getText().equalsIgnoreCase( "nose" ) || text.getText().equalsIgnoreCase( "origin" ) )
		{
			if ( wormOrigin == null )
			{
				wormOrigin = new Vector3f( text.elementAt(0) );
//				updateLattice(true);
			}
			else
			{
				wormOrigin.copy( text.elementAt(0) );
//				updateLattice(false);
			}
		}
    }
    
    private void addInsertionPoint( Vector3f startPt, Vector3f endPt, Vector3f maxPt )
    {
    	Segment3f mouseVector = new Segment3f( startPt, endPt );
    	float minDistL = Float.MAX_VALUE;
    	int minIndexL = -1;
    	Vector3f newLeft = null;
    	for ( int i = 0; i < left.size() -1; i++ )
    	{
    		Segment3f leftS = new Segment3f( left.elementAt(i), left.elementAt(i+1) );
    		DistanceSegment3Segment3 dist = new DistanceSegment3Segment3( mouseVector, leftS );
    		float distance = dist.Get();
    		if ( distance < minDistL )
    		{
    			minDistL = distance;
    			if ( minDistL <= 12 )
    			{
//    				System.err.println( dist.GetSegment0Parameter() + " " + dist.GetSegment1Parameter() );
    				minIndexL = i;
    				newLeft = Vector3f.add( leftS.Center, Vector3f.scale( dist.GetSegment1Parameter(), leftS.Direction ) );
    				newLeft.copy(maxPt);
    			}
    		}
    	}
    	float minDistR = Float.MAX_VALUE;
    	int minIndexR = -1;
    	Vector3f newRight = null;
    	for ( int i = 0; i < left.size() -1; i++ )
    	{
    		Segment3f rightS = new Segment3f( right.elementAt(i), right.elementAt(i+1) );
    		DistanceSegment3Segment3 dist = new DistanceSegment3Segment3( mouseVector, rightS );
    		float distance = dist.Get();
    		if ( distance < minDistR )
    		{
    			minDistR = distance;
    			if ( minDistR <= 12 )
    			{
//    				System.err.println( dist.GetSegment0Parameter() + " " + dist.GetSegment1Parameter() );
    				minIndexR = i;
    				newRight = Vector3f.add( rightS.Center, Vector3f.scale( dist.GetSegment1Parameter(), rightS.Direction ) );
    				newRight.copy(maxPt);
    			}
    		}
    	}
    	if ( (minIndexL != -1) && (minIndexR != -1) )
    	{
    		if ( minDistL < minDistR )
    		{
//    			System.err.println( "Add to left " + (minIndexL+1) );
    			left.add( minIndexL + 1, newLeft );
    			pickedPoint = left.elementAt(minIndexL+1);
    			newRight = Vector3f.add( right.elementAt(minIndexL), right.elementAt(minIndexL+1) );
    			newRight.scale(0.5f);
    			right.add( minIndexL + 1, newRight );
    			
            	updateLattice(true);
    		}
    		else
    		{
//    			System.err.println( "Add to right " + (minIndexR+1) );
    			right.add( minIndexR + 1, newRight );
    			pickedPoint = right.elementAt(minIndexR+1);
    			newLeft = Vector3f.add( left.elementAt(minIndexR), left.elementAt(minIndexR+1) );
    			newLeft.scale(0.5f);
    			left.add( minIndexR + 1, newLeft );
    			
            	updateLattice(true);
    		}
    	}
    	else if ( minIndexL != -1 )
    	{
//			System.err.println( "Add to left " + (minIndexL+1) );
			left.add( minIndexL + 1, newLeft );
			pickedPoint = left.elementAt(minIndexL+1);
			newRight = Vector3f.add( right.elementAt(minIndexL), right.elementAt(minIndexL+1) );
			newRight.scale(0.5f);
			right.add( minIndexL + 1, newRight );
			
        	updateLattice(true);
    	}
    	else if ( minIndexR != -1 )
    	{
//			System.err.println( "Add to right " + (minIndexR+1) );
			right.add( minIndexR + 1, newRight );
			pickedPoint = right.elementAt(minIndexR+1);
			newLeft = Vector3f.add( left.elementAt(minIndexR), left.elementAt(minIndexR+1) );
			newLeft.scale(0.5f);
			left.add( minIndexR + 1, newLeft );  
			
        	updateLattice(true);
    	}
    }
    
    public void addLeftRightMarker( Vector3f pt )
    {
    	if ( lattice == null )
    	{
			short id = (short) imageA.getVOIs().getUniqueID();
			lattice = new VOI(id, "lattice", VOI.POLYLINE, (float)Math.random() );
    		
    		left = new VOIContour(false);
    		right = new VOIContour(false);
    		lattice.getCurves().add(left);
    		lattice.getCurves().add(right);
    		
    		this.imageA.registerVOI(lattice);
    	}
    	if ( left.size() == right.size() )
    	{
    		left.add( new Vector3f(pt) );
    		pickedPoint = left.lastElement();
//    		System.err.println( pt );
    		
    		if ( leftMarker == null )
    		{
    			short id = (short) imageA.getVOIs().getUniqueID();
    			leftMarker = new VOI(id, "leftMarker", VOI.POINT, (float)Math.random() );
    			this.imageA.registerVOI(leftMarker);
    			leftMarker.importPoint( pt );
    		}
    		else
    		{
    			leftMarker.getCurves().elementAt(0).elementAt(0).copy(pt);
    			leftMarker.update();
    		}
    		return;
    	}
    	else
    	{
    		right.add( new Vector3f(pt) );
    		pickedPoint = right.lastElement();
//    		System.err.println( pt );
    		
    		if ( rightMarker == null )
    		{
    			short id = (short) imageA.getVOIs().getUniqueID();
    			rightMarker = new VOI(id, "rightMarker", VOI.POINT, (float)Math.random() );
    			this.imageA.registerVOI(rightMarker);
        		rightMarker.importPoint( pt );
    		}
    		else
    		{
    			rightMarker.getCurves().elementAt(0).elementAt(0).copy(pt);
    			rightMarker.update();
    		}
    	}
//    	if ( left.size() == right.size() && left.size() > 1 )
    	{
    		updateLattice(true);
    	}
    }

    
    public void clear3DSelection()
    {
    	pickedPoint = null;
    	pickedAnnotation = -1;
    	if ( showSelected != null )
    	{
    		imageA.unregisterVOI(showSelectedVOI);
    	}
    	VOIVector vois = imageA.getVOIs();
    	for ( int i = vois.size() - 1; i >= 0; i-- )
    	{
    		VOI voi = vois.elementAt(i);
    		String name = voi.getName();
    		if ( name.equals("showSelected") )
    		{
//        		System.err.println( "clear3DSelection " + vois.elementAt(i).getName() );
    			imageA.unregisterVOI(voi);
    		}
    	}
    }


    public void clearAddLeftRightMarkers()
    {
    	imageA.unregisterVOI( leftMarker );
    	imageA.unregisterVOI( rightMarker );
    	if ( leftMarker != null )
    	{
    		leftMarker.dispose();
        	leftMarker = null;
    	}
    	if ( rightMarker != null )
    	{
    		rightMarker.dispose();
    		rightMarker = null;
    	}
    }
    

    public void deleteSelectedPoint( boolean doAnnotation )
    {
    	if ( doAnnotation )
    	{
        	if ( pickedAnnotation != -1 )
        	{
        		VOIText text = (VOIText)annotationVOIs.getCurves().remove(pickedAnnotation);
    			clear3DSelection();

        		if ( text.getText().equalsIgnoreCase( "nose" ) || text.getText().equalsIgnoreCase( "origin" ) )
        		{
        			wormOrigin = null;
//        			updateLattice(true);
        		}
        	}    		
    	}
    	else if ( !doAnnotation )
    	{
    		boolean deletedLeft = false;
    		boolean deletedRight = false;
    		if ( (rightMarker != null) && pickedPoint.equals(rightMarker.getCurves().elementAt(0).elementAt(0) ) )
    		{
    			imageA.unregisterVOI( rightMarker );
    			rightMarker.dispose();
    			rightMarker = null;
    			
    			deletedRight = true;
    		}

    		if ( (leftMarker != null) && pickedPoint.equals(leftMarker.getCurves().elementAt(0).elementAt(0) ) )
    		{
    			imageA.unregisterVOI( leftMarker );
    			leftMarker.dispose();
    			leftMarker = null;
    			deletedLeft = true;

    			if ( rightMarker != null )
    			{
    				imageA.unregisterVOI( rightMarker );
    				rightMarker.dispose();
    				rightMarker = null;
    				deletedRight = true;
    			}
    		}
    		if ( deletedLeft || deletedRight )
    		{
    			if ( deletedLeft )
    			{
    				left.remove(left.lastElement() );
    			}
    			if ( deletedRight )
    			{
    				right.remove(right.lastElement() );
    			}
    		}
    		else
    		{
        		int leftIndex = left.indexOf(pickedPoint);
        		int rightIndex = right.indexOf(pickedPoint);
    			if ( leftIndex != -1 )
    			{
    				left.remove(leftIndex);
    				right.remove(leftIndex);
        			deletedLeft = true;
    				deletedRight = true;
    			}
    			else if ( rightIndex != -1 )
    			{
    				left.remove(rightIndex);
    				right.remove(rightIndex);
        			deletedLeft = true;
    				deletedRight = true;
    			}
    		}
			clear3DSelection();
    		updateLattice(deletedLeft | deletedRight );
    	}
    	pickedPoint = null;
    	pickedAnnotation = -1;
    }
       
    
    public void dispose()
	{
		if ( latticeGrid != null )
		{
			for ( int i = latticeGrid.size() - 1; i >= 0; i-- )
			{
				VOI marker = latticeGrid.remove(i);
				imageA.unregisterVOI( marker );
			}
		}
		imageA.unregisterVOI(lattice);
		imageA.unregisterVOI( displayContours );
		imageA.unregisterVOI(leftLine);
		imageA.unregisterVOI(rightLine);
		imageA.unregisterVOI(centerLine);
		clear3DSelection();
		
		imageA = null;
		latticeGrid = null;
		lattice = null;
		left = null;
		right = null;
		center = null;
		afTimeC = null;
		allTimes = null;
		centerSpline = null;
		leftSpline = null;
		rightSpline = null;
		centerPositions = null;	    
		leftPositions = null;
		rightPositions = null;	    
		leftLine = null;
		rightLine = null;
		centerLine = null;		

//		if ( centerTangents != null )
//			centerTangents.clear();
//		centerTangents = null;
		
		if ( wormDiameters != null )
			wormDiameters.clear();
		wormDiameters = null;
		
		if ( rightVectors != null )
			rightVectors.clear();
		rightVectors = null;
		
		if ( upVectors != null )
			upVectors.clear();
		upVectors = null;

		if ( boxBounds != null )
			boxBounds.clear();
		boxBounds = null;
		
		if ( ellipseBounds != null )
			ellipseBounds.clear();
		ellipseBounds = null;

		samplingPlanes = null;
		displayContours = null;    
		pickedPoint = null;
		showSelectedVOI = null;
		showSelected = null;
	}
    
    
    public Vector3f getPicked()
    {
    	return pickedPoint;
    }
    
    
    
    public Vector3f getPicked( Vector3f pt, boolean doAnnotation )
    {
    	pickedPoint = null;
    	
    	if ( doAnnotation )
    	{
    		if ( annotationVOIs == null )
    		{
    			return null;
    		}
        	pickedAnnotation = -1;
    		float minDist = Float.MAX_VALUE;
        	for ( int i = 0; i < annotationVOIs.getCurves().size(); i++ )
        	{
        		Vector3f annotationPt = annotationVOIs.getCurves().elementAt(i).elementAt(0);
        		float distance = pt.distance(annotationPt);
        		if ( distance < minDist )
        		{
        			minDist = distance;
        			if ( minDist <= 12 )
        			{
        				pickedAnnotation = i;
        			}
        		}
        	}
        	if ( pickedAnnotation != -1 )
        	{
        		pickedPoint = annotationVOIs.getCurves().elementAt(pickedAnnotation).elementAt(0);
        	}    		
    	}
    	else
    	{
    		if ( left == null )
    		{
    			return pickedPoint;
    		}
    		int closestL = -1;
    		float minDistL = Float.MAX_VALUE;
    		for ( int i = 0; i < left.size(); i++ )
    		{
    			float distance = pt.distance(left.elementAt(i));
    			if ( distance < minDistL )
    			{
    				minDistL = distance;
    				if ( minDistL <= 12 )
    				{
    					closestL = i;
    				}
    			}
    		}
    		int closestR = -1;
    		float minDistR = Float.MAX_VALUE;
    		if ( right != null )
    		{
    			for ( int i = 0; i < right.size(); i++ )
    			{
    				float distance = pt.distance(right.elementAt(i));
    				if ( distance < minDistR )
    				{
    					minDistR = distance;
    					if ( minDistR <= 12 )
    					{
    						closestR = i;
    					}
    				}
    			}
    		}

    		//    	System.err.println( minDistL + " " + minDistR );
    		if ( (closestL != -1) && (closestR != -1) )
    		{
    			if ( minDistL < minDistR )
    			{
    				//    			System.err.println( "Picked Lattice Left " + closestL );
    				pickedPoint = left.elementAt(closestL);
    			}
    			else
    			{
    				//    			System.err.println( "Picked Lattice Right " + closestR );
    				pickedPoint = right.elementAt(closestR);
    			}
    		}
    		else if ( closestL != -1 )
    		{
    			//			System.err.println( "Picked Lattice Left " + closestL );
    			pickedPoint = left.elementAt(closestL);
    		}
    		else if ( closestR != -1 )
    		{
    			//			System.err.println( "Picked Lattice Right " + closestR );
    			pickedPoint = right.elementAt(closestR);
    		}

    		if ( pickedPoint != null )
    		{
    			updateLattice(false);
    		}
    	}
    	return pickedPoint;
    }
    
    ModelImage markerSegmentation;
	int[][] markerVolumes;
	int[] markerIDs;
	boolean[] completedIDs;
	int[] currentID;
    public void interpolateLattice( boolean displayResult )
	{

        leftBackup = new VOIContour(false);
        rightBackup = new VOIContour(false);
        for ( int i = 0; i < left.size(); i++ )
        {
        	leftBackup.add(new Vector3f(left.elementAt(i)));
        	rightBackup.add(new Vector3f(right.elementAt(i)));
        }   	    	
    	
		float[] closestTimes = new float[afTimeC.length];
		float[] leftDistances = new float[afTimeC.length];
		float[] rightDistances = new float[afTimeC.length];
		for ( int i = 0; i < afTimeC.length; i++ )
		{
			float minDif = Float.MAX_VALUE;
			for ( int j = 0; j < allTimes.length; j++ )
			{
				float dif = Math.abs(allTimes[j] - afTimeC[i]);
				if ( dif < minDif )
				{
					minDif = dif;
					closestTimes[i] = allTimes[j];
				}
			}
			leftDistances[i] = 0;
			rightDistances[i] = 0;
			if ( i > 0 )
			{
				rightDistances[i] = rightSpline.GetLength(closestTimes[i-1], closestTimes[i]);
				leftDistances[i] = leftSpline.GetLength(closestTimes[i-1], closestTimes[i]);
			}
		}

		saveLatticeStatistics(imageA, length, left, right, leftDistances, rightDistances, "_before");
		saveAnnotationStatistics( imageA, null, null, null, "_before");
		

        // modify markers based on volume segmentation:
    	markerVolumes = new int[left.size()][2];
    	markerIDs = new int[left.size()];
    	completedIDs = new boolean[left.size()];
    	currentID = new int[]{0};
    	markerSegmentation = segmentMarkers( imageA, left, right, markerIDs, markerVolumes, false );
		for ( int i = 0; i < completedIDs.length; i++ )
		{
			if ( markerIDs[i] == 0 )
			{
				completedIDs[i] = true;
			}
		}
		generateCurves();
		
		

		boxBounds = new Vector<Box3f>();
		ellipseBounds = new Vector<Ellipsoid3f>();
		short sID = (short)(imageA.getVOIs().getUniqueID());
		samplingPlanes = new VOI(sID, "samplingPlanes");
		for ( int i = 0; i < centerPositions.size(); i++ )
		{
	        Vector3f rkEye = centerPositions.elementAt(i);
	        Vector3f rkRVector = rightVectors.elementAt(i);
	        Vector3f rkUVector = upVectors.elementAt(i);
	        
			Vector3f[] output = new Vector3f[4];
	        Vector3f rightV = Vector3f.scale( extent, rkRVector );
	        Vector3f upV = Vector3f.scale( extent, rkUVector );
	        output[0] = Vector3f.add( Vector3f.neg(rightV), Vector3f.neg(upV) );
	        output[1] = Vector3f.add( rightV, Vector3f.neg(upV) );
	        output[2] = Vector3f.add( rightV, upV );
	        output[3] = Vector3f.add( Vector3f.neg(rightV), upV );
	        for ( int j = 0; j < 4; j++ )
	        {
	        	output[j].add(rkEye);
	        }
			VOIContour kBox = new VOIContour(true);
			for ( int j = 0; j < 4; j++ )
			{
				kBox.addElement( output[j].X, output[j].Y, output[j].Z );
			}
			kBox.update( new ColorRGBA(0,0,1,1) );		
	        {	
	        	samplingPlanes.importCurve(kBox);
	        }
	        

	        float curve = centerSpline.GetCurvature(allTimes[i]);
	        float scale = curve;
	        VOIContour ellipse = new VOIContour(true);
	        Ellipsoid3f ellipsoid = makeEllipse( rkRVector, rkUVector, rkEye, wormDiameters.elementAt(i), scale, ellipse );
	        ellipseBounds.add(ellipsoid);
	        
	        
	        Box3f box = new Box3f( ellipsoid.Center, ellipsoid.Axis, new float[]{extent, extent, 1 } );
	        boxBounds.add(box);
		}
//		System.err.println( "interpolateLattice " + extent );
		
		generateMasks( imageA, imageB, samplingPlanes, ellipseBounds, wormDiameters, 2*extent, true, displayResult  );
	}
    
//    public TriMesh makeModel( boolean fullModel )
//	{						
//		float scale = 1;
//		int numPts = 6;
//		double[] adCos = new double[numPts];
//		double[] adSin = new double[numPts];
//		for ( int i = 0; i < numPts; i++ )
//		{
//			adCos[i] = Math.cos( Math.PI * 2.0 * i/numPts );
//			adSin[i] = Math.sin( Math.PI * 2.0 * i/numPts);
//		}
//		
//		int numSlices = left.size();
//		if ( fullModel )
//		{
//			numSlices = centerPositions.size();
//		}
//		Attributes attributes = new Attributes();
//		attributes.SetPChannels(3);
//		attributes.SetNChannels(3);
//		attributes.SetCChannels(0,3);
//		attributes.SetTChannels(0, 2);
//		VertexBuffer vBuffer = new VertexBuffer( attributes, numSlices * numPts + 2 );
//		IndexBuffer iBuffer = new IndexBuffer( (vBuffer.GetVertexQuantity() - 2 ) * 6 );
//		int vCount = 0;
//		int nCount = 0;
//		int cCount = 0;
//		int iCount = 0;
//		int tCount = 0;
//		int gCount = 0;
//		int z = 0;
//		
//    	boolean[] grow = new boolean[vBuffer.GetVertexQuantity() ];
//		for ( int i = 0; i < numSlices; i++ )
//		{
//			float tx = (z++ + (numSlices/2))/(float)(numSlices-1);
//			int index = i;
//			if ( !fullModel )
//			{
//				index = latticeSlice[i];
//			}
//			
//			
//			Vector3f rkEye = centerPositions.elementAt(index);
//			Vector3f rkRVector = rightVectors.elementAt(index);
//			Vector3f rkUVector = upVectors.elementAt(index);
//	        Vector3f rkDVector = centerTangents.elementAt(index);
//	        scale = wormDiameters.elementAt(index);
//			if ( i == 0 )
//			{
//				vBuffer.SetPosition3(vCount++, rkEye);
//				vBuffer.SetNormal3(nCount++, Vector3f.neg(rkDVector) );
//				vBuffer.SetColor4(0, cCount++, 0, 0, 1, 1 );
//				vBuffer.SetTCoord2(0, tCount++, tx, 0 );
//				grow[gCount++] = false;
//				for ( int j = 0; j < numPts; j++ )
//				{
//					Vector3f pos1 = Vector3f.scale((float) (scale * adCos[j]), rkRVector);
//					Vector3f pos2 = Vector3f.scale((float) (scale * adSin[j]), rkUVector);
//					Vector3f pos = Vector3f.add(pos1,pos2);
//					pos.add(rkEye);
//					Vector3f normal = Vector3f.sub( pos, rkEye );
//					normal.normalize();
//					vBuffer.SetPosition3(vCount++, pos);
//					vBuffer.SetNormal3(nCount++, normal );
//					vBuffer.SetTCoord2(0, tCount++, tx, j / (float)(numPts-1) );
//					vBuffer.SetColor4(0, cCount++, 0, 0, 1, 1 );
//					grow[gCount++] = true;
//					
//					if ( j < (numPts-1) )
//					{
//						iBuffer.GetData()[iCount++] = vCount - 1;
//						iBuffer.GetData()[iCount++] = vCount;
//						iBuffer.GetData()[iCount++] = 0;
//						
//						iBuffer.GetData()[iCount++] = vCount;
//						iBuffer.GetData()[iCount++] = vCount - 1;
//						iBuffer.GetData()[iCount++] = vCount - 1 + numPts;
//
//						iBuffer.GetData()[iCount++] = vCount;
//						iBuffer.GetData()[iCount++] = vCount - 1 + numPts;
//						iBuffer.GetData()[iCount++] = vCount + numPts;
//					}
//					else
//					{
//						iBuffer.GetData()[iCount++] = vCount - 1;
//						iBuffer.GetData()[iCount++] = vCount - numPts;
//						iBuffer.GetData()[iCount++] = 0;
//						
//						iBuffer.GetData()[iCount++] = vCount - numPts;
//						iBuffer.GetData()[iCount++] = vCount - 1;
//						iBuffer.GetData()[iCount++] = vCount - 1 + numPts;
//
//						iBuffer.GetData()[iCount++] = vCount - numPts;
//						iBuffer.GetData()[iCount++] = vCount - 1 + numPts;
//						iBuffer.GetData()[iCount++] = vCount;
//					}
//				}
//			}
//			else if ( i == (numSlices - 1) )
//			{				
//				int centerIndex = vBuffer.GetVertexQuantity() - 1;
//				for ( int j = 0; j < numPts; j++ )
//				{
//					Vector3f pos1 = Vector3f.scale((float) (scale * adCos[j]), rkRVector);
//					Vector3f pos2 = Vector3f.scale((float) (scale * adSin[j]), rkUVector);
//					Vector3f pos = Vector3f.add(pos1,pos2);
//					pos.add(rkEye);
//					Vector3f normal = Vector3f.sub( pos, rkEye );
//					normal.normalize();
//					vBuffer.SetPosition3(vCount++, pos);
//					vBuffer.SetNormal3(nCount++, normal );
//					vBuffer.SetTCoord2(0, tCount++, tx, j / (float)(numPts-1) );
//					vBuffer.SetColor4(0, cCount++, 0, 0, 1, 1 );
//					grow[gCount++] = true;
//					
//					if ( j < (numPts-1) )
//					{
//						iBuffer.GetData()[iCount++] = vCount;
//						iBuffer.GetData()[iCount++] = vCount - 1;
//						iBuffer.GetData()[iCount++] = centerIndex;
//					}
//					else
//					{
//						iBuffer.GetData()[iCount++] = vCount - numPts;
//						iBuffer.GetData()[iCount++] = vCount - 1;
//						iBuffer.GetData()[iCount++] = centerIndex;
//					}
//				}
//
//				vBuffer.SetPosition3(vCount++, rkEye);
//				vBuffer.SetNormal3(nCount++, rkDVector );
//				grow[gCount++] = false;
//			}
//			else
//			{				
//				for ( int j = 0; j < numPts; j++ )
//				{
//					Vector3f pos1 = Vector3f.scale((float) (scale * adCos[j]), rkRVector);
//					Vector3f pos2 = Vector3f.scale((float) (scale * adSin[j]), rkUVector);
//					Vector3f pos = Vector3f.add(pos1,pos2);
//					pos.add(rkEye);
//					Vector3f normal = Vector3f.sub( pos, rkEye );
//					normal.normalize();
//					vBuffer.SetPosition3(vCount++, pos);
//					vBuffer.SetNormal3(nCount++, normal );
//					vBuffer.SetTCoord2(0, tCount++, tx, j / (float)(numPts-1) );
//					vBuffer.SetColor4(0, cCount++, 0, 0, 1, 1 );
//					grow[gCount++] = true;
//					
//					if ( j < (numPts-1) )
//					{
//						iBuffer.GetData()[iCount++] = vCount;
//						iBuffer.GetData()[iCount++] = vCount - 1;
//						iBuffer.GetData()[iCount++] = vCount - 1 + numPts;
//
//						iBuffer.GetData()[iCount++] = vCount;
//						iBuffer.GetData()[iCount++] = vCount - 1 + numPts;
//						iBuffer.GetData()[iCount++] = vCount + numPts;
//					}
//					else
//					{
//						iBuffer.GetData()[iCount++] = vCount - numPts;
//						iBuffer.GetData()[iCount++] = vCount - 1;
//						iBuffer.GetData()[iCount++] = vCount - 1 + numPts;
//
//						iBuffer.GetData()[iCount++] = vCount - numPts;
//						iBuffer.GetData()[iCount++] = vCount - 1 + numPts;
//						iBuffer.GetData()[iCount++] = vCount;
//					}
//				}
//			}
//		}
//		System.err.println( iCount + " " + iBuffer.GetData().length );
//		System.err.println( vCount + " " + vBuffer.GetVertexQuantity() );
//		TriMesh mesh = new TriMesh(vBuffer, iBuffer, false);
////		System.err.println( "Scale mesh = full " + testIntersections(mesh) );
////		growMesh( mesh, wormDiameters, grow );
//		
//		return mesh;
//	}
    
    public void modifyAnnotation( Vector3f startPt, Vector3f endPt, Vector3f pt )
    {
    	if ( annotationVOIs == null )
    	{
    		return;
    	}
    	if ( annotationVOIs.getCurves().size() == 0 )
    	{
    		return;
    	}
    	if ( pickedPoint != null )
    	{
    		Vector3f diff = Vector3f.sub( pt, pickedPoint );
    		pickedPoint.copy(pt);
    		annotationVOIs.getCurves().elementAt(pickedAnnotation).elementAt(1).add(diff);
    		annotationVOIs.getCurves().elementAt(pickedAnnotation).update();
    	}
    	else
    	{
    		pickedPoint = null;
    		pickedAnnotation = -1;
    		float minDist = Float.MAX_VALUE;
    		for ( int i = 0; i < annotationVOIs.getCurves().size(); i++ )
    		{
    			Vector3f annotationPt = annotationVOIs.getCurves().elementAt(i).elementAt(0);
    			float distance = pt.distance(annotationPt);
    			if ( distance < minDist )
    			{
    				minDist = distance;
    				if ( minDist <= 12 )
    				{
    					pickedAnnotation = i;
    				}
    			}
    		}
    	}
    	if ( pickedAnnotation != -1 )
    	{
    		pickedPoint = annotationVOIs.getCurves().elementAt(pickedAnnotation).elementAt(0);
        	updateSelected();

        	VOIText text = (VOIText) annotationVOIs.getCurves().elementAt(pickedAnnotation);
    		if ( text.getText().equalsIgnoreCase( "nose" ) || text.getText().equalsIgnoreCase( "origin" ) )
    		{
    			wormOrigin.copy( pickedPoint );
//    			updateLattice(false);
    		}
    	}
    	
    }
    
    public void modifyLattice( Vector3f startPt, Vector3f endPt, Vector3f pt )
    {
    	if ( pickedPoint != null )
    	{
    		pickedPoint.copy(pt);
        	updateLattice(false);
        	return;
    	}
    	pickedPoint = null;
    	int closestL = -1;
		float minDistL = Float.MAX_VALUE;
    	for ( int i = 0; i < left.size(); i++ )
    	{
    		float distance = pt.distance(left.elementAt(i));
    		if ( distance < minDistL )
    		{
    			minDistL = distance;
    			if ( minDistL <= 12 )
    			{
    				closestL = i;
    			}
    		}
    	}
    	int closestR = -1;
		float minDistR = Float.MAX_VALUE;
    	for ( int i = 0; i < right.size(); i++ )
    	{
    		float distance = pt.distance(right.elementAt(i));
    		if ( distance < minDistR )
    		{
    			minDistR = distance;
    			if ( minDistR <= 12 )
    			{
    				closestR = i;
    			}
    		}
    	}
//    	System.err.println( minDistL + " " + minDistR );
    	if ( (closestL != -1) && (closestR != -1) )
    	{
    		if ( minDistL < minDistR )
    		{
//    			System.err.println( "Picked Lattice Left " + closestL );
    			pickedPoint = left.elementAt(closestL);
    		}
    		else
    		{
//    			System.err.println( "Picked Lattice Right " + closestR );
    			pickedPoint = right.elementAt(closestR);
    		}
    	}
    	else if ( closestL != -1 )
    	{
//			System.err.println( "Picked Lattice Left " + closestL );
    		pickedPoint = left.elementAt(closestL);
    	}
    	else if ( closestR != -1 )
    	{
//			System.err.println( "Picked Lattice Right " + closestR );
    		pickedPoint = right.elementAt(closestR);
    	}
    	if ( pickedPoint != null )
    	{
        	updateLattice(false);
        	return;
    	}
    	// look at the vector under the mouse and see which lattice point is closest...
    	Segment3f mouseVector = new Segment3f( startPt, endPt );
    	float minDist = Float.MAX_VALUE;
    	for ( int i = 0; i < left.size(); i++ )
    	{
    		DistanceVector3Segment3 dist = new DistanceVector3Segment3( left.elementAt(i), mouseVector );
    		float distance = dist.Get();
    		if ( distance < minDist )
    		{
    			minDist = distance;
    			pickedPoint = left.elementAt(i);
    		}
    		dist = new DistanceVector3Segment3( right.elementAt(i), mouseVector );
    		distance = dist.Get();
    		if ( distance < minDist )
    		{
    			minDist = distance;
    			pickedPoint = right.elementAt(i);
    		}
    	}
		if ( (pickedPoint != null) && (minDist <= 12) )
		{
        	updateLattice(false);
        	return;
		}
    	
    	addInsertionPoint(startPt, endPt, pt );
    }
    
    
//    public void interpolate( ModelImage srcImage, ModelImage destImage, VOI samplingPlanes, Vector<Ellipsoid3f> ellipseBounds, int diameter )
//    {
//		int dimX = srcImage.getExtents().length > 0 ? srcImage.getExtents()[0] : 1;
//		int dimY = srcImage.getExtents().length > 1 ? srcImage.getExtents()[1] : 1;
//		int[] resultExtents = new int[]{dimX, dimY, samplingPlanes.getCurves().size()};
//
//		float[] values = new float[dimX*dimY];		
//		
//		System.err.println( dimX + "  ==?   " + diameter );
//		
//		System.err.println( srcImage.getExtents()[2] + "  ==?   " + samplingPlanes.getCurves().size() );
//		int end = Math.min( srcImage.getExtents()[2], samplingPlanes.getCurves().size() );
//		for( int i = 0; i < end; i++ )
//		{
////			float diameterInterp = samplingDiameters.elementAt(i);
//			VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
//	        Vector3f[] corners = new Vector3f[4];
//	        for ( int j = 0; j < 4; j++ )
//	        {
//	        	corners[j] = kBox.elementAt(j);
//	        }
//			try {
//				srcImage.exportData( i * values.length, values.length, values );
//				destImage.writeDiagonal( 0, i, resultExtents, corners, ellipseBounds.elementAt(i), values );
//			} catch(IOException e) {
//				e.printStackTrace();
//			}
//		}
//		
//		destImage.calcMinMax();
//    }
    
    
//    public void interpolateImages( ModelImage srcImage, ModelImage destImage, VOI destLattice )
//	{
//
//		// Assume image is isotropic (square voxels).
//		if ( destLattice.getCurves().size() != 2 )
//		{
//			return;
//		}
//		VOIContour left = (VOIContour) destLattice.getCurves().elementAt(0);
//		VOIContour right = (VOIContour) destLattice.getCurves().elementAt(1);
//		if ( left.size() != right.size() )
//		{
//			return;
//		}
//		VOIContour center = new VOIContour(false);
//		for ( int i = 0; i < left.size(); i++ )
//		{
//			Vector3f centerPt = Vector3f.add(left.elementAt(i), right.elementAt(i) );
//			centerPt.scale(0.5f);
//			center.add(centerPt);
//		}
//
//    	float[] afTimeC = new float[center.size()];
//		NaturalSpline3 centerSpline = smoothCurve(center, afTimeC);
//		NaturalSpline3 leftSpline = smoothCurve2(left, afTimeC);
//		NaturalSpline3 rightSpline = smoothCurve2(right, afTimeC);
//
//		
//		Vector<Vector3f> centerPositions = new Vector<Vector3f>();
//		Vector<Vector3f> centerTangents = new Vector<Vector3f>();
//		Vector<Float> wormDiameters = new Vector<Float>();
//		Vector<Vector3f> rightVectors = new Vector<Vector3f>();
//		Vector<Vector3f> upVectors = new Vector<Vector3f>();
//		
//		float length = centerSpline.GetLength(0, 1);
//		int srcLength = srcImage.getExtents()[2];
//		int extent = 0;
//		float[] allTimes = new float[srcLength];
//		float minCurve = Float.MAX_VALUE;
//		float maxCurve = -Float.MAX_VALUE;
//		for ( int i = 0; i < srcLength; i++ )
//		{
//			int timeIndex = (int) ((length-1) * i/(srcLength - 1));
////			System.err.println( i + " " + timeIndex );
//			float t = centerSpline.GetTime(timeIndex);
//			allTimes[i] = t;
//			centerPositions.add(centerSpline.GetPosition(t));
//			centerTangents.add( centerSpline.GetFirstDerivative(t) );
//			Vector3f leftPt = leftSpline.GetPosition(t);
//			Vector3f rightPt = rightSpline.GetPosition(t);
//			
//			Vector3f rightDir = Vector3f.sub( rightPt, leftPt );		
//			float diameter = rightDir.normalize();
//			diameter /= 2f;
//			diameter += DiameterBuffer;
//			if ( diameter > extent )
//			{
//				extent = (int) Math.ceil(diameter);
//			}			
//			wormDiameters.add(diameter);
//			rightVectors.add(rightDir);
//			
//			centerTangents.elementAt(i).normalize();
//			Vector3f upDir = Vector3f.cross( rightDir, centerTangents.elementAt(i) );
//			upDir.normalize();
//			upVectors.add(upDir);
////			if ( i > 0 )
////			{
////				System.err.println( i + "   " + centerPositions.elementAt(i).distance(centerPositions.elementAt(i-1)));
////			}
//			float curve = centerSpline.GetSecondDerivative(t).length();
//        	if ( curve < minCurve )
//        	{
//        		minCurve = curve;
//        	}
//        	if ( curve > maxCurve )
//        	{
//        		maxCurve = curve;
//        	}
//
//		}		
//		extent += 10;
//		
//		Vector<Ellipsoid3f> ellipseBounds = new Vector<Ellipsoid3f>();
//		short sID = (short)(destImage.getVOIs().getUniqueID());
//		VOI samplingPlanes = new VOI(sID, "samplingPlanes");
//		VOI wormContours = new VOI(sID, "wormContours");
//		for ( int i = 0; i < centerPositions.size(); i++ )
//		{
//	        Vector3f rkEye = centerPositions.elementAt(i);
//	        Vector3f rkRVector = rightVectors.elementAt(i);
//	        Vector3f rkUVector = upVectors.elementAt(i);
//	        
//			Vector3f[] output = new Vector3f[4];
//	        Vector3f rightV = Vector3f.scale( extent, rkRVector );
//	        Vector3f upV = Vector3f.scale( extent, rkUVector );
//	        output[0] = Vector3f.add( Vector3f.neg(rightV), Vector3f.neg(upV) );
//	        output[1] = Vector3f.add( rightV, Vector3f.neg(upV) );
//	        output[2] = Vector3f.add( rightV, upV );
//	        output[3] = Vector3f.add( Vector3f.neg(rightV), upV );
//	        for ( int j = 0; j < 4; j++ )
//	        {
//	        	output[j].add(rkEye);
//	        }
//
//	        float curve = centerSpline.GetSecondDerivative(allTimes[i]).length();
//	        float scale = (curve - minCurve)/(maxCurve - minCurve);
//	        VOIContour ellipse = new VOIContour(true);
//	        Ellipsoid3f ellipsoid = makeEllipse( rkRVector, rkUVector, rkEye, wormDiameters.elementAt(i), scale, ellipse );
////	        System.err.println( i + " " + rkEye );
//	        ellipseBounds.add( ellipsoid );
//	        wormContours.importCurve(ellipse);
//
//	        VOIContour kBox = new VOIContour(true);
//			for ( int j = 0; j < 4; j++ )
//			{
//				kBox.addElement( output[j].X, output[j].Y, output[j].Z );
//			}
////			System.err.println( kBox.elementAt(0).distance( kBox.elementAt(1) ) + " " + kBox.elementAt(2).distance( kBox.elementAt(3) ) );
////			System.err.println( kBox.elementAt(0).distance( kBox.elementAt(3) ) + " " + kBox.elementAt(1).distance( kBox.elementAt(2) ) );
//			kBox.update( new ColorRGBA(0,0,1,1) );		
////	        if ( (i%40) == 0 )
//	        {	
//	        	samplingPlanes.importCurve(kBox);
//	        }
//		}
//		VOIContour centerLine = new VOIContour(false);
//		centerLine.addAll( centerPositions );
//		sID = (short)(destImage.getVOIs().getUniqueID());
//		VOI samplingPoints = new VOI(sID, "samplingPlanes");
//		samplingPoints.getCurves().add(centerLine);
//		destImage.registerVOI(samplingPoints);
//		
//		interpolate(srcImage, destImage, samplingPlanes, ellipseBounds, 2*extent );
//	}
    
    
    public void moveSelectedPoint( Vector3f direction, boolean doAnnotation )
    {
    	if ( pickedPoint != null )
    	{
    		pickedPoint.add(direction);
    		if ( doAnnotation && (pickedAnnotation != -1) )
    		{
        		annotationVOIs.getCurves().elementAt(pickedAnnotation).elementAt(1).add(direction);
        		annotationVOIs.getCurves().elementAt(pickedAnnotation).update();
    			updateSelected();
    		}
    		else
    		{
    			updateLattice(false);
    		}
    	}    	
    }
    
    
    public void redo()
    {
    	updateLinks();
    }
    
    public void saveAnnotations( )
    {
        final JFileChooser chooser = new JFileChooser();

        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        } else {
            chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
        }

        chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        final int returnVal = chooser.showSaveDialog(null);

        String fileName = null, directory = null, voiDir;
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();
            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            Preferences.setProperty(Preferences.PREF_VOI_LPS_SAVE, "true");
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
        }

        if (fileName != null) {
            voiDir = new String(directory + fileName + File.separator);   
            
            clear3DSelection();
            
			imageA.unregisterAllVOIs();
			imageA.registerVOI( annotationVOIs );			
			saveAllVOIsTo( voiDir, imageA );    

			imageA.unregisterAllVOIs();
			imageA.registerVOI(annotationVOIs);
			if ( leftMarker != null )
			{
				imageA.registerVOI(leftMarker);
			}
			if ( rightMarker != null )
			{
				imageA.registerVOI(rightMarker);
			}
			if ( lattice != null )
			{
				imageA.registerVOI( lattice );
			}
			updateLattice(true);
        }

    }
    
    public void saveLattice( )
    {
        final JFileChooser chooser = new JFileChooser();

        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        } else {
            chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
        }

        chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        final int returnVal = chooser.showSaveDialog(null);

        String fileName = null, directory = null, voiDir;
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();
            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            Preferences.setProperty(Preferences.PREF_VOI_LPS_SAVE, "true");
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
        }

        if (fileName != null) {
            voiDir = new String(directory + fileName + File.separator);   
            
            clear3DSelection();
            
			imageA.unregisterAllVOIs();
			imageA.registerVOI(lattice);
			lattice.setColor( new Color( 0, 0, 255) );
			lattice.getCurves().elementAt(0).update( new ColorRGBA(0,0,1,1));
			lattice.getCurves().elementAt(1).update( new ColorRGBA(0,0,1,1));
			lattice.getCurves().elementAt(0).setClosed(false);
			lattice.getCurves().elementAt(1).setClosed(false);
			for ( int j = 0; j < lattice.getCurves().elementAt(0).size(); j++ )
			{
				short id = (short) imageA.getVOIs().getUniqueID();
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
				imageA.registerVOI( marker );
			}
			
			saveAllVOIsTo( voiDir, imageA );    

			imageA.unregisterAllVOIs();
			imageA.registerVOI(lattice);
			if ( leftMarker != null )
			{
				imageA.registerVOI(leftMarker);
			}
			if ( rightMarker != null )
			{
				imageA.registerVOI(rightMarker);
			}
			if ( annotationVOIs != null )
			{
				imageA.registerVOI( annotationVOIs );
			}
			updateLattice(true);
        }

    }

    
    public void setAnnotations( VOI newAnnotations )
    {
    	if ( annotationVOIs != null )
    	{
    		imageA.unregisterVOI( annotationVOIs );    		
    	}
    	annotationVOIs = newAnnotations;
    	if ( pickedAnnotation != -1 )
    	{
    		clear3DSelection();
    	}
    	clearAddLeftRightMarkers();
    	
    	for ( int i = 0; i < annotationVOIs.getCurves().size(); i++ )
    	{
        	VOIText text = (VOIText) annotationVOIs.getCurves().elementAt(i);
        	Color c = text.getColor();
        	text.update( new ColorRGBA( c.getRed()/255.0f, c.getGreen()/255.0f, c.getBlue()/255.0f, 1f ));
        	text.elementAt(1).copy( text.elementAt(0) );
        	text.elementAt(1).add( 6, 0, 0 );
        	
			if ( text.getText().equalsIgnoreCase( "nose" ) || text.getText().equalsIgnoreCase( "origin" ) )
			{
				if ( wormOrigin == null )
				{
					wormOrigin = new Vector3f( text.elementAt(0) );
//					updateLattice(true);
				}
				else
				{
					wormOrigin.copy( text.elementAt(0) );
//					updateLattice(false);
				}
			}
    	}
    }
    
    public void setLattice( VOI newLattice )
    {
    	if ( lattice != null )
    	{
    		imageA.unregisterVOI( lattice );
    	}
		this.lattice = newLattice;

		// Assume image is isotropic (square voxels).
		if ( lattice.getCurves().size() != 2 )
		{
			return;
		}
		left = (VOIContour) lattice.getCurves().elementAt(0);
		right = (VOIContour) lattice.getCurves().elementAt(1);
		if ( left.size() != right.size() )
		{
			return;
		}
		
		this.imageA.registerVOI(lattice);
		clear3DSelection();
		clearAddLeftRightMarkers();
		updateLattice(true);
    }
	
	public void setPicked( Vector3f pt, boolean doAnnotation )
    {
    	if ( pickedPoint == null )
    	{
    		return;
    	}
    	
    	if ( doAnnotation && (pickedAnnotation != -1) )
    	{
    		Vector3f diff = Vector3f.sub( pt, pickedPoint );
    		pickedPoint.copy(pt);
    		annotationVOIs.getCurves().elementAt(pickedAnnotation).elementAt(1).add(diff);
    		annotationVOIs.getCurves().elementAt(pickedAnnotation).update();
        	updateSelected();    		
    	}
    	else if ( !doAnnotation )
    	{
    		if ( (leftMarker != null) && pickedPoint.equals(leftMarker.getCurves().elementAt(0).elementAt(0) ) )
    		{
    			leftMarker.getCurves().elementAt(0).elementAt(0).copy(pt);
    			leftMarker.update();
    		}
    		if ( (rightMarker != null) && pickedPoint.equals(rightMarker.getCurves().elementAt(0).elementAt(0) ) )
    		{
    			rightMarker.getCurves().elementAt(0).elementAt(0).copy(pt);
    			rightMarker.update();
    		}
    		pickedPoint.copy(pt);
    		updateLattice(false);
    	}
    }

	public ModelImage segmentMarkers( ModelImage image, VOIContour left, VOIContour right, int[] markerIDs, int[][] markerVolumes, boolean segAll )
	{
    	String imageName = image.getImageName();
    	if ( imageName.contains("_clone") )
    	{
    		imageName = imageName.replaceAll("_clone", "" );
    	}
		ModelImage markerSegmentation = new ModelImage(ModelStorageBase.FLOAT, image.getExtents(), imageName + "_markers.xml");
		JDialogBase.updateFileInfo( image, markerSegmentation );		
		
		ModelImage gmImage = VolumeImage.getGradientMagnitude( image, 0 );
		
		float maxValue = -Float.MAX_VALUE;
    	for ( int i = 0; i < left.size(); i++ )
    	{
    		Vector3f temp = right.elementAt(i);
        	int x = Math.round(temp.X);
        	int y = Math.round(temp.Y);
        	int z = Math.round(temp.Z);
    		float value;
    		if ( image.isColorImage() )
    		{
    			value = image.getFloatC( x, y, z, 2); // Green Channel contains markers
    		}
    		else
    		{
    			value = image.getFloat( x, y, z );
//    			value = image.getFloatTriLinearBounds(temp.X, temp.Y, temp.Z);
    		}
    		if ( value > maxValue )
    		{
    			maxValue = value;
    		}
    		temp = left.elementAt(i);
        	x = Math.round(temp.X);
        	y = Math.round(temp.Y);
        	z = Math.round(temp.Z);
    		if ( image.isColorImage() )
    		{
    			value = image.getFloatC( x, y, z, 2); // Green Channel contains markers
    		}
    		else
    		{
    			value = image.getFloat( x, y, z );
//    			value = image.getFloatTriLinearBounds(temp.X, temp.Y, temp.Z);
    		}
    		if ( value > maxValue )
    		{
    			maxValue = value;
    		}
    	}
    	
    	float minOverall = Float.MAX_VALUE;
    	float[][] minDistance = new float[left.size()][2];
    	for ( int i = 0; i < left.size(); i++ )
    	{
    		minDistance[i][0] = Float.MAX_VALUE;
    		Vector3f tempL = left.elementAt(i);
    		for ( int j = i + 1; j < left.size(); j++ )
    		{
    			float dist = tempL.distance( left.elementAt(j) );
    			if ( dist < minDistance[i][0] )
    			{
    				minDistance[i][0] = dist;
    			}
    			if ( dist < minOverall )
    			{
    				minOverall = dist;
    			}
    		}
    		for ( int j = 0; j < right.size(); j++ )
    		{
    			float dist = tempL.distance( right.elementAt(j) );
    			if ( dist < minDistance[i][0] )
    			{
    				minDistance[i][0] = dist;
    			}
    			if ( dist < minOverall )
    			{
    				minOverall = dist;
    			}
    		}
    	}
    	for ( int i = 0; i < right.size(); i++ )
    	{
    		minDistance[i][1] = Float.MAX_VALUE;
    		Vector3f tempR = right.elementAt(i);
    		for ( int j = i + 1; j < right.size(); j++ )
    		{
    			float dist = tempR.distance( right.elementAt(j) );
    			if ( dist < minDistance[i][1] )
    			{
    				minDistance[i][1] = dist;
    			}
    			if ( dist < minOverall )
    			{
    				minOverall = dist;
    			}
    		}
    		for ( int j = 0; j < left.size(); j++ )
    		{
    			float dist = tempR.distance( left.elementAt(j) );
    			if ( dist < minDistance[i][1] )
    			{
    				minDistance[i][1] = dist;
    			}
    			if ( dist < minOverall )
    			{
    				minOverall = dist;
    			}
    		}
    	}
    	
    	minOverall *= 0.75;
    	int step = (int) (minOverall / 3f);
//    	System.err.println( step + " " + minDistance );

    	Vector<Vector3f> seedList = new Vector<Vector3f>();
    	VOIContour[][] savedSeedList = new VOIContour[left.size()][2];
    	int[][] counts = new int[left.size()][2];
    	for ( int diameter = step; diameter <= minOverall; diameter += step )
    	{
    		for ( int i = 0; i < left.size(); i++ )
    		{
//    			int diameter = (int) (minDistance[i][0]/2f);
    			if ( savedSeedList[i][0] == null )
    			{
    				savedSeedList[i][0] = new VOIContour(false);
    				seedList.clear();
    				seedList.add(new Vector3f( left.elementAt(i) ));
    				counts[i][0] = fill( image, gmImage, markerSegmentation, 10, 0.1f*maxValue, new Vector3f( left.elementAt(i) ), seedList, savedSeedList[i][0], diameter, i+1 );
//    				System.err.println( "left " + i + " " + counts[i][0] );
    			}
    			else
    			{	
    				seedList.clear();
    				counts[i][0] += fill( image, gmImage, markerSegmentation, 10, 0.1f*maxValue, new Vector3f( left.elementAt(i) ), savedSeedList[i][0], seedList, diameter, i+1 );
    				savedSeedList[i][0].clear();
    				savedSeedList[i][0].addAll(seedList);
//    				System.err.println( "left " + i + " " + counts[i][0] );
    			}
    		}
    		for ( int i = 0; i < right.size(); i++ )
    		{
//    			int diameter = (int) (minDistance[i][1]/2f);
    			if ( savedSeedList[i][1] == null )
    			{
    				savedSeedList[i][1] = new VOIContour(false);
    				seedList.clear();
    				seedList.add(new Vector3f( right.elementAt(i) ));
    				counts[i][1] = fill( image, gmImage, markerSegmentation, 10, 0.1f*maxValue, new Vector3f( right.elementAt(i) ), seedList, savedSeedList[i][0], diameter, i+1 );
//    				System.err.println( "right " + i + " " + counts[i][1] );
    			}
    			else
    			{	
    				seedList.clear();
    				counts[i][1] += fill( image, gmImage, markerSegmentation, 10, 0.1f*maxValue, new Vector3f( right.elementAt(i) ), savedSeedList[i][0], seedList, diameter, i+1 );
    				savedSeedList[i][1].clear();
    				savedSeedList[i][1].addAll(seedList);
//    				System.err.println( "right " + i + " " + counts[i][1] );
    			}
    		}
    	}
    	for ( int i = 0; i < left.size(); i++ )
    	{
    		markerVolumes[i][0] = counts[i][0];
    		markerVolumes[i][1] = counts[i][1];
    	}
		saveLatticeStatistics( imageA, null, null, left, right, markerVolumes, "_before" );

    	for ( int i = 0; i < left.size(); i++ )
    	{
    		if ( counts[i][0] == 0 )
    		{
//    			markerIDs[i] = 0;
				Vector3f dir = Vector3f.sub( right.elementAt(i), left.elementAt(i) );
				dir.normalize();
				dir.scale(step);
				seedList.clear();
				Vector3f newPt = Vector3f.add( left.elementAt(i), dir );
				seedList.add( newPt );
				savedSeedList[i][0].clear();
				counts[i][0] = fill( image, gmImage, markerSegmentation, 10, 0.1f*maxValue, newPt, seedList, savedSeedList[i][0], (int) minOverall, i+1 );
				if ( counts[i][0] == 0 )
				{
					seedList.clear();
					seedList.add(new Vector3f( left.elementAt(i) ));
					savedSeedList[i][0].clear();
					counts[i][0] = fill( image, gmImage, markerSegmentation, 0, 0, new Vector3f( left.elementAt(i) ), seedList, savedSeedList[i][0], step, i+1 );
				}
				markerIDs[i] = i+1;
				moveMarker( markerSegmentation, left.elementAt(i), Vector3f.sub( left.elementAt(i), right.elementAt(i)), i+1 );
    		}
    		else
    		{
    			markerIDs[i] = i+1;
    			moveMarker( markerSegmentation, left.elementAt(i), Vector3f.sub( left.elementAt(i), right.elementAt(i)), i+1 );
    		}
    	}
    	for ( int i = 0; i < right.size(); i++ )
    	{
    		if ( counts[i][1] == 0 )
    		{
//    			markerIDs[i] = 0;
				Vector3f dir = Vector3f.sub( left.elementAt(i), right.elementAt(i) );
				dir.normalize();
				dir.scale(step);
				seedList.clear();
				Vector3f newPt = Vector3f.add( right.elementAt(i), dir );
				seedList.add( newPt );
				savedSeedList[i][1].clear();
				counts[i][1] = fill( image, gmImage, markerSegmentation, 10, 0.1f*maxValue, newPt, seedList, savedSeedList[i][0], (int) minOverall, i+1 );
				if ( counts[i][1] == 0 )
				{
					seedList.clear();
					seedList.add(new Vector3f( right.elementAt(i) ));
					savedSeedList[i][1].clear();
					counts[i][1] = fill( image, gmImage, markerSegmentation, 0, 0, new Vector3f( right.elementAt(i) ), seedList, savedSeedList[i][1], step, i+1 );
					markerIDs[i] = i+1;
					moveMarker( markerSegmentation, right.elementAt(i), Vector3f.sub( right.elementAt(i), left.elementAt(i)), i+1 );
				}
    		}
    		else
    		{
    			markerIDs[i] = i+1;
    			moveMarker( markerSegmentation, right.elementAt(i), Vector3f.sub( right.elementAt(i), left.elementAt(i)), i+1 );
    		}
    	}
    	markerSegmentation.calcMinMax();
//		new ViewJFrameImage((ModelImage)markerSegmentation.clone());
		
    	if ( segAll )
    	{
    		ModelImage markerSegmentation2 = new ModelImage(ModelStorageBase.FLOAT, image.getExtents(), imageName + "_markers2.xml");
    		JDialogBase.updateFileInfo( image, markerSegmentation2 );		


    		seedList.clear();
    		for ( int i = 0; i < left.size(); i++ )
    		{
    			seedList.add(new Vector3f( left.elementAt(i) ));
    			seedList.add(new Vector3f( right.elementAt(i) ));

    			if ( markerVolumes[i][0] == 0 )
    			{
    				Vector3f dir = Vector3f.sub( right.elementAt(i), left.elementAt(i) );
    				dir.normalize();
    				dir.scale(step);
    				Vector3f newPt = Vector3f.add( left.elementAt(i), dir );
    				seedList.add( newPt );
    			}
    			if ( markerVolumes[i][1] == 0 )
    			{
    				Vector3f dir = Vector3f.sub( left.elementAt(i), right.elementAt(i) );
    				dir.normalize();
    				dir.scale(step);
    				Vector3f newPt = Vector3f.add( right.elementAt(i), dir );
    				seedList.add( newPt );
    			}
    		}

    		savedSeedList[0][0].clear();
    		int count = fill( image, gmImage, markerSegmentation2, 10, 0.1f*maxValue, Vector3f.ZERO, seedList, savedSeedList[0][0], Integer.MAX_VALUE, (int) (.75*(image.getMax() - image.getMin())) );

    		System.err.println( "segment all " + count );
    		markerSegmentation2.calcMinMax();
    		new ViewJFrameImage((ModelImage)markerSegmentation2.clone());
    	}
		
		

    	gmImage.disposeLocal();
    	gmImage = null;
    	
    	
		return markerSegmentation;
	}
	
	
	private void moveMarker( ModelImage markerImage, Vector3f pt, Vector3f dir, int id )
	{
    	int dimX = markerImage.getExtents().length > 0 ? markerImage.getExtents()[0] : 1;
    	int dimY = markerImage.getExtents().length > 1 ? markerImage.getExtents()[1] : 1;
    	int dimZ = markerImage.getExtents().length > 2 ? markerImage.getExtents()[2] : 1;
    	
		dir.normalize();
		Vector3f temp = new Vector3f(pt);
		temp.add(dir);
		int x = Math.round(temp.X);
		int y = Math.round(temp.Y);
		int z = Math.round(temp.Z);
		if ( (x < 0) || (x >= dimX) || (y < 0) || (y >= dimY) || (z < 0) || (z >= dimZ) )
		{
			return;
		}
		float value = markerImage.getFloat(x,y,z);
		while ( value == id )
		{
			pt.copy(temp);
			temp.add(dir);
			x = Math.round(temp.X);
			y = Math.round(temp.Y);
			z = Math.round(temp.Z);
			if ( (x < 0) || (x >= dimX) || (y < 0) || (y >= dimY) || (z < 0) || (z >= dimZ) )
			{
				return;
			}
			value = markerImage.getFloat(x,y,z);
		}
	}
	

    private int fill( ModelImage image, ModelImage gmImage, ModelImage model, float gmMin, float intensityMin, Vector3f centerPt, 
    		Vector<Vector3f> seedList, Vector<Vector3f> saveSeedList,
    		
    		int maxDiameter, int id )
    {
    	int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
    	int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
    	int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
    	
    	double averageValue = 0;
    	int count = 0;

    	
    	while ( seedList.size() > 0 )
    	{
    		Vector3f seed = seedList.remove(0);
    		if ( centerPt.distance(seed) > maxDiameter )
    		{
    			saveSeedList.add(seed);
    			continue;
    		}
    		
    		int z = Math.round(seed.Z);
    		int y = Math.round(seed.Y);
    		int x = Math.round(seed.X);
    		float value = model.getFloat(x,y,z);
    		if ( value != 0 )
    		{
    			continue;
    		}
    		float valueGM;
    		if ( image.isColorImage() )
    		{
    			value = image.getFloatC(x, y, z, 2);
    			valueGM = gmImage.getFloatC(x, y, z, 2);
    		}
    		else
    		{
    			value = image.getFloat(x, y, z);
    			valueGM = gmImage.getFloat(x, y, z);
    		}
			if ( (value >= intensityMin) && (valueGM >= gmMin) )
			{
				for ( int z1 = Math.max(0, z-1); z1 <= Math.min(dimZ-1, z+1); z1++ )
				{
					for ( int y1 = Math.max(0, y-1); y1 <= Math.min(dimY-1, y+1); y1++ )
					{
						for ( int x1 = Math.max(0, x-1); x1 <= Math.min(dimX-1, x+1); x1++ )
						{
							if ( !((x == x1) && (y == y1) && (z == z1)) )
							{
								if ( image.isColorImage() )
								{
									value = image.getFloatC(x1, y1, z1, 2);
									valueGM = gmImage.getFloatC(x1, y1, z1, 2);
								}
								else
								{
									value = image.getFloat(x1, y1, z1);
									valueGM = gmImage.getFloat(x1, y1, z1);
								}
								if ( value >= intensityMin )
								{
									seedList.add( new Vector3f(x1,y1,z1) );
								}
							}
						}
					}
				}
				count++;
				model.set(x, y, z, id);
	    		if ( image.isColorImage() )
	    		{
	    			value = image.getFloatC(x, y, z, 2);
	    		}
	    		else
	    		{
	    			value = image.getFloat(x, y, z);	    			
	    		}
				averageValue += value;
			}
    	}
//    	if ( count != 0 )
//    	{
//    		averageValue /= (float)count;
//    		System.err.println( "fill markers " + count + " " + (float)averageValue + " " + (float)(averageValue/image.getMax()) );
//    	}
    	return count;
    }
	
	

	public void showInterpolatedModel()
	{
//    	int[] markerIDs = new int[left.size()];
//		markerSegmentation = segmentMarkers( markerIDs );
//		for ( int i = 0; i < left.size(); i++ )
//		{
//			if ( markerIDs[i] != 0 )
//			{
//				Vector3f dir = Vector3f.sub(right.elementAt(i), left.elementAt(i));
//				dir.normalize();
//				Vector3f pt = left.elementAt(i);
//				float value = markerSegmentation.getFloat( (int)pt.X, (int)pt.Y, (int)pt.Z );
//				while ( value == i+1 )
//				{
//					pt.sub(dir);
//					value = markerSegmentation.getFloat( (int)pt.X, (int)pt.Y, (int)pt.Z );
//				}
//				pt.add(dir);
//				pt = right.elementAt(i);
//				value = markerSegmentation.getFloat( (int)pt.X, (int)pt.Y, (int)pt.Z );
//				while ( value == i+1 )
//				{
//					pt.add(dir);
//					value = markerSegmentation.getFloat( (int)pt.X, (int)pt.Y, (int)pt.Z );
//				}
//				pt.sub(dir);
//			}
//		}
//    	updateLattice(false);
//
//		markerSegmentation = segmentMarkers( markerIDs );
		
//    	String imageName = imageA.getImageName();
//    	if ( imageName.contains("_clone") )
//    	{
//    		imageName = imageName.replaceAll("_clone", "" );
//    	}
//		ModelImage midLineSegmentation = new ModelImage(ModelStorageBase.FLOAT, imageA.getExtents(), imageName + "_model.xml");
//		JDialogBase.updateFileInfo( imageA, midLineSegmentation );	
//		
//
//		float maxValue = -Float.MAX_VALUE;
//    	for ( int i = 0; i < left.size(); i++ )
//    	{
//    		Vector3f temp = right.elementAt(i);
//    		float value = imageA.getFloatTriLinearBounds(temp.X, temp.Y, temp.Z);
//    		if ( value > maxValue )
//    		{
//    			maxValue = value;
//    		}
//    		temp = left.elementAt(i);
//    		value = imageA.getFloatTriLinearBounds(temp.X, temp.Y, temp.Z);
//    		if ( value > maxValue )
//    		{
//    			maxValue = value;
//    		}
//    	}
//		
//		for ( int i = 0; i < centerPositions.size(); i++ )
//		{
//	        Vector3f rkRVector = rightVectors.elementAt(i);
//	        Vector3f rkUVector = upVectors.elementAt(i);
//			Vector3f[] axis = new Vector3f[3];
//			axis[0] = rkRVector;
//			axis[1] = rkUVector;
//			axis[2] = Vector3f.cross( rkRVector, rkUVector );
//	        
//	        float size = rightPositions.elementAt(i).distance( leftPositions.elementAt(i) );
//	        size /= 6;
//	        
//	        Box3f box = new Box3f( centerPositions.elementAt(i), axis, new float[]{size, size, size } );
//	        fill( imageA, midLineSegmentation, markers, 0.01f*maxValue, box );
//		}
//		
//		midLineSegmentation.calcMinMax();
//		new ViewJFrameImage(midLineSegmentation);
	}
	
	private void fill( ModelImage image, ModelImage model, ModelImage markers, float intensityMin, Box3f box )
	{
		Vector3f min = new Vector3f();
		Vector3f max = new Vector3f();
		box.ComputeBounds(min, max);
		Vector3f pt = new Vector3f();
		int count = 0;
		for ( int z = (int)min.Z; z <= max.Z; z++ )
		{
			for ( int y = (int)min.Y; y <= max.Y; y++ )
			{
				for ( int x = (int)min.X; x <= max.X; x++ )
				{
					pt.set(x, y, z);
					if ( ContBox3f.InBox( pt, box ) )
					{
						float value = image.getFloat( x, y, z );
						if ( value >= intensityMin )
						{
							value = markers.getFloat( x, y, z );
							if ( value == 0 )
							{
								count++;
								model.set(x, y, z, 1);
							}
						}
					}
				}
			}
		}
		System.err.println( "fill " + count );
	}
	
	public void showInterpolatedModel2()
    {	
		boxBounds = new Vector<Box3f>();
		ellipseBounds = new Vector<Ellipsoid3f>();
		short sID = (short)(imageA.getVOIs().getUniqueID());

//		VOIContour topLeft = new VOIContour(false);
//		VOIContour topRight = new VOIContour(false);
//		VOIContour bottomLeft = new VOIContour(false);
//		VOIContour bottomRight = new VOIContour(false);

//		sID = (short)(imageA.getVOIs().getUniqueID());
//		VOI crossLines = new VOI(sID, "crossLines");
//		crossLines.setColor( Color.red );
//		imageA.registerVOI(crossLines);
		
		samplingPlanes = new VOI(sID, "samplingPlanes");
		for ( int i = 0; i < centerPositions.size(); i++ )
		{
	        Vector3f rkEye = centerPositions.elementAt(i);
	        Vector3f rkRVector = rightVectors.elementAt(i);
	        Vector3f rkUVector = upVectors.elementAt(i);
	        
			Vector3f[] output = new Vector3f[4];
	        Vector3f rightV = Vector3f.scale( extent, rkRVector );
	        Vector3f upV = Vector3f.scale( extent, rkUVector );
	        output[0] = Vector3f.add( Vector3f.neg(rightV), Vector3f.neg(upV) );
	        output[1] = Vector3f.add( rightV, Vector3f.neg(upV) );
	        output[2] = Vector3f.add( rightV, upV );
	        output[3] = Vector3f.add( Vector3f.neg(rightV), upV );
	        for ( int j = 0; j < 4; j++ )
	        {
	        	output[j].add(rkEye);
	        }
	        
//	        bottomRight.add( output[0] );
//	        bottomLeft.add( output[1] );
//	        topLeft.add( output[2] );
//	        topRight.add( output[3] );
//	        
//	        if ( (i%30) == 0 )
//	        {
//	        	VOIContour cross = new VOIContour(false);
//	        	cross.add(output[0]);
//	        	cross.add(output[1]);
//	        	crossLines.getCurves().add(cross);
//	        	cross.update( new ColorRGBA(1,0,0,1));
//	        }
	        
			VOIContour kBox = new VOIContour(true);
			for ( int j = 0; j < 4; j++ )
			{
				kBox.addElement( output[j].X, output[j].Y, output[j].Z );
			}
			kBox.update( new ColorRGBA(0,0,1,1) );		
	        {	
	        	samplingPlanes.importCurve(kBox);
	        }
	        

	        float curve = centerSpline.GetCurvature(allTimes[i]);
	        float scale = curve;
	        VOIContour ellipse = new VOIContour(true);
	        Ellipsoid3f ellipsoid = makeEllipse( rkRVector, rkUVector, rkEye, wormDiameters.elementAt(i), scale, ellipse );
	        ellipseBounds.add(ellipsoid);
	        
	        
	        Box3f box = new Box3f( ellipsoid.Center, ellipsoid.Axis, new float[]{extent, extent, 1 } );
	        boxBounds.add(box);
		}
//		sID = (short)(imageA.getVOIs().getUniqueID());
//		VOI topLeftLine = new VOI(sID, "topLeftLine");
//		topLeftLine.getCurves().add(topLeft);
//		topLeftLine.setColor( Color.red );
//		topLeft.update( new ColorRGBA(1,0,0,1));
//		imageA.registerVOI(topLeftLine);
//
//		sID = (short)(imageA.getVOIs().getUniqueID());
//		VOI topRightLine = new VOI(sID, "topRightLine");
//		topRightLine.getCurves().add(topRight);
//		topRightLine.setColor( Color.green );
//		topRight.update( new ColorRGBA(0,1,0,1));
//		imageA.registerVOI(topRightLine);
		

//		sID = (short)(imageA.getVOIs().getUniqueID());
//		VOI bottomLeftLine = new VOI(sID, "bottomLeftLine");
//		bottomLeftLine.getCurves().add(bottomLeft);
//		bottomLeftLine.setColor( Color.blue );
//		bottomLeft.update( new ColorRGBA(0,0,1,1));
//		imageA.registerVOI(bottomLeftLine);
//		
//
//		sID = (short)(imageA.getVOIs().getUniqueID());
//		VOI bottomRightLine = new VOI(sID, "bottomRightLine");
//		bottomRightLine.getCurves().add(bottomRight);
//		bottomRightLine.setColor( Color.magenta );
//		bottomRight.update( new ColorRGBA(1,0,1,1));
//		imageA.registerVOI(bottomRightLine);
//		
//		clearCurves();
//		imageA.unregisterVOI( lattice );
		
//		generateMasks( imageA, imageB, samplingPlanes, ellipseBounds, wormDiameters, 2*extent, false, false  );
    }
	
    public void showModel( )
	{
		if ( (imageA.isRegistered( displayContours ) == -1) )
		{
			imageA.registerVOI(displayContours);
	        imageA.notifyImageDisplayListeners();
//	        showModel = true;
		}
		else if ( (imageA.isRegistered( displayContours ) != -1) )
		{
			imageA.unregisterVOI(displayContours);
	        imageA.notifyImageDisplayListeners();
//	        showModel = false;
		}
		
	}
	
    public void undo()
    {
    	updateLinks();
    }
    
    
    private void testOriginToStraight( ModelImage model, ModelImage originToStraight )
    {
//    	ModelImage test = (ModelImage) model.clone();
    	
    	int dimX = model.getExtents().length > 0 ? model.getExtents()[0] : 1;
    	int dimY = model.getExtents().length > 1 ? model.getExtents()[1] : 1;
    	int dimZ = model.getExtents().length > 2 ? model.getExtents()[2] : 1;

    	int missing1 = 0;
    	int missing2 = 0;
    	int modelCount = 0;
    	
    	Vector3f pts = new Vector3f();
    	for ( int z = 0; z < dimZ; z++ )
    	{
    		for ( int y = 0; y < dimY; y++ )
    		{
    			for ( int x = 0; x < dimX; x++ )
    			{
    				float a = originToStraight.getFloatC(x, y, z, 0);
    				float m = model.getFloat(x,y,z);
    				if ( m != 0 )
    				{
    					modelCount++;
    				}
    				if ( (a == 0) && (m != 0) )
    				{
    					missing1++;
    					
    					int count = 0;
    					pts.set(0,0,0);
    					for ( int z1 = Math.max(0, z-1); z1 < Math.min(dimZ, z+1); z1++ )
    					{
    						for ( int y1 = Math.max(0, y-1); y1 < Math.min(dimY, y+1); y1++ )
    						{
    							for ( int x1 = Math.max(0, x-1); x1 < Math.min(dimX, x+1); x1++ )
    	    					{
    			    				float a1 = originToStraight.getFloatC(x1, y1, z1, 0);
    								float m1 = model.getFloat(x1,y1,z1);
    								if ( (a1 != 0) && (m1 == m) )
    								{
        			    				float x2 = originToStraight.getFloatC(x1, y1, z1, 1);
        			    				float y2 = originToStraight.getFloatC(x1, y1, z1, 2);
        			    				float z2 = originToStraight.getFloatC(x1, y1, z1, 3);
        			    				pts.add( x2,y2,z2 );
        			    				count++;
    								}
    	    					}
    						}
    					}
    					if ( count != 0 )
    					{
    						pts.scale(1f/(float)count);
    						originToStraight.setC(x, y, z, 0, 1);
    						originToStraight.setC(x, y, z, 1, pts.X);
    						originToStraight.setC(x, y, z, 2, pts.Y);
    						originToStraight.setC(x, y, z, 3, pts.Z);
//        					test.set(x,y,z,0);
    					}
    					else 
    					{
//    						test.set(x,y,z,1);
    						missing2++;
    					}
    				}
    				if ( (a != 0) && (m != 0) )
    				{
//    					test.set(x,y,z,0);
    				}
    			}
    		}
    	}
//    	System.err.println( modelCount + " " + missing1 + " " + missing2 );
//    	System.err.println( missing1/(float)modelCount + " " + missing2/(float)modelCount );    	
//    	
//    	test.calcMinMax();
//    	new ViewJFrameImage(test);
    }
    


	private void exportDiagonal( ModelImage model, ModelImage markerSegmentation,
			float[] sliceIDs,
			int[] markerIDs, boolean[] completedIDs, int[] currentID, 
    		final int tSlice, final int slice, final int[] extents,
            final Vector3f[] verts, final Ellipsoid3f ellipseBound) 
    {
        final int iBound = extents[0];
        final int jBound = extents[1];

        int[] dimExtents = markerSegmentation.getExtents();
        
        /*
         * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
         * coordinate-systems: transformation:
         */
        final int iFactor = 1;
        final int jFactor = dimExtents[0];
        final int kFactor = dimExtents[0] * dimExtents[1];
        final int tFactor = dimExtents[0] * dimExtents[1] * dimExtents[2];

        int buffFactor = 1;
        
        Vector3f center = new Vector3f();
        for ( int i = 0; i < verts.length; i++ )
        {
        	center.add(verts[i]);
        }
        center.scale( 1f/verts.length );
        
        /* Calculate the slopes for traversing the data in x,y,z: */
        float xSlopeX = verts[1].X - verts[0].X;
        float ySlopeX = verts[1].Y - verts[0].Y;
        float zSlopeX = verts[1].Z - verts[0].Z;

        float xSlopeY = verts[3].X - verts[0].X;
        float ySlopeY = verts[3].Y - verts[0].Y;
        float zSlopeY = verts[3].Z - verts[0].Z;

        float x0 = verts[0].X;
        float y0 = verts[0].Y;
        float z0 = verts[0].Z;

        xSlopeX /= (iBound);
        ySlopeX /= (iBound);
        zSlopeX /= (iBound);

        xSlopeY /= (jBound);
        ySlopeY /= (jBound);
        zSlopeY /= (jBound);

        /* loop over the 2D image (values) we're writing into */
        float x = x0;
        float y = y0;
        float z = z0;
        
        Vector3f currentPoint = new Vector3f();

        float[] values = new float[iBound * jBound];
        for (int j = 0; j < jBound; j++) {

            /* Initialize the first diagonal point(x,y,z): */
            x = x0;
            y = y0;
            z = z0;

            for (int i = 0; i < iBound; i++) {
            	values[j * iBound + i] = 0;
                final int iIndex = Math.round(x);
                final int jIndex = Math.round(y);
                final int kIndex = Math.round(z);

                /* calculate the ModelImage space index: */
                final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));

                // Bounds checking:
                if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
                		|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > markerSegmentation.getSize()))) {

                	// do nothing
                } else {
                    currentPoint.set(x, y, z);
                    boolean isInside = ellipseBound.Contains(currentPoint);
            		float currentValue = model.getFloat(iIndex, jIndex, kIndex);
//            		float currentValue = model.getFloatTriLinearBounds(x, y, z);
                    if ( isInside && (currentValue != 0) )
                    {
                    	values[j * iBound + i] = markerSegmentation.getFloat( iIndex, jIndex, kIndex );
                    }
                }

                /*
                 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
                 * ySlopeX and zSlopeX values:
                 */
                x = x + xSlopeX;
                y = y + ySlopeX;
                z = z + zSlopeX;
            }

            /*
             * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
             * ySlopeY and zSlopeY values:
             */
            x0 = x0 + xSlopeY;
            y0 = y0 + ySlopeY;
            z0 = z0 + zSlopeY;
        }              
        
        
        float markerPt = 0;

		boolean inconsistent = false;
		for ( int j = 0; j < values.length && !inconsistent; j++ )
		{
			if ( values[j] > 0 )
			{
    			for ( int k = j+1; k < values.length && !inconsistent; k++ )
    			{
    				if ( (values[k] != 0) && (values[j] != values[k]) )
    				{
    					inconsistent = true;
    					break;
    				}
    			}    
    			markerPt = values[j];
			}
		}
		if ( inconsistent || (markerPt == 0) )
		{
			return;
		}
		
		if ( (markerIDs[ currentID[0] ] < markerPt) && completedIDs[ currentID[0] ] )
		{
			for ( int i = currentID[0] + 1; i < markerIDs.length; i++ )
			{
				if ( markerIDs[i] == 0 )
				{
					continue;
				}
				else
				{
					currentID[0] = i;
					break;
				}
			}
		}
		
		if ( markerIDs[ currentID[0] ] != markerPt )
		{
			return;
		}
		completedIDs[ currentID[0] ] = true;
		
		sliceIDs[slice] = markerPt;
//		System.err.println( slice + " " + markerPt + " " + markerIDs[ currentID[0] ] );
    }
		
	

	private void exportDiagonal( ModelImage model, ModelImage markerSegmentation,
			float[] sliceIDs, 
    		final int tSlice, final int slice, final int[] extents,
            final Vector3f[] verts, final Ellipsoid3f ellipseBound) 
    {
        final int iBound = extents[0];
        final int jBound = extents[1];

        int[] dimExtents = markerSegmentation.getExtents();
        
        /*
         * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
         * coordinate-systems: transformation:
         */
        final int iFactor = 1;
        final int jFactor = dimExtents[0];
        final int kFactor = dimExtents[0] * dimExtents[1];
        final int tFactor = dimExtents[0] * dimExtents[1] * dimExtents[2];

        int buffFactor = 1;
        
        Vector3f center = new Vector3f();
        for ( int i = 0; i < verts.length; i++ )
        {
        	center.add(verts[i]);
        }
        center.scale( 1f/verts.length );
        
        /* Calculate the slopes for traversing the data in x,y,z: */
        float xSlopeX = verts[1].X - verts[0].X;
        float ySlopeX = verts[1].Y - verts[0].Y;
        float zSlopeX = verts[1].Z - verts[0].Z;

        float xSlopeY = verts[3].X - verts[0].X;
        float ySlopeY = verts[3].Y - verts[0].Y;
        float zSlopeY = verts[3].Z - verts[0].Z;

        float x0 = verts[0].X;
        float y0 = verts[0].Y;
        float z0 = verts[0].Z;

        xSlopeX /= (iBound);
        ySlopeX /= (iBound);
        zSlopeX /= (iBound);

        xSlopeY /= (jBound);
        ySlopeY /= (jBound);
        zSlopeY /= (jBound);

        /* loop over the 2D image (values) we're writing into */
        float x = x0;
        float y = y0;
        float z = z0;
	

        Vector3f currentPoint = new Vector3f();
        for (int j = 0; j < jBound; j++) {

            /* Initialize the first diagonal point(x,y,z): */
            x = x0;
            y = y0;
            z = z0;

            for (int i = 0; i < iBound; i++) {
                final int iIndex = Math.round(x);
                final int jIndex = Math.round(y);
                final int kIndex = Math.round(z);

                /* calculate the ModelImage space index: */
                final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));

                // Bounds checking:
                if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
                		|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > markerSegmentation.getSize()))) {

                	// do nothing
                } else {
                    currentPoint.set(x, y, z);
                    boolean isInside = ellipseBound.Contains(currentPoint);
            		float currentValue = model.getFloat(iIndex, jIndex, kIndex);
//            		float currentValue = model.getFloatTriLinearBounds(x, y, z);
                    if ( isInside )
//                        if ( isInside && (currentValue != 0) )
                    {
                    	markerSegmentation.set( iIndex, jIndex, kIndex, sliceIDs[slice] );
                    }
                }

                /*
                 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
                 * ySlopeX and zSlopeX values:
                 */
                x = x + xSlopeX;
                y = y + ySlopeX;
                z = z + zSlopeX;
            }

            /*
             * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
             * ySlopeY and zSlopeY values:
             */
            x0 = x0 + xSlopeY;
            y0 = y0 + ySlopeY;
            z0 = z0 + zSlopeY;
        }              
    }

	private void exportDiagonal( ModelImage model, ModelImage markerSegmentation,
			float[] sliceIDs, 
    		final int tSlice, final int slice, final int[] extents,
            final Vector3f[] verts, float value) 
    {
        final int iBound = extents[0];
        final int jBound = extents[1];

        int[] dimExtents = markerSegmentation.getExtents();
        
        /*
         * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
         * coordinate-systems: transformation:
         */
        final int iFactor = 1;
        final int jFactor = dimExtents[0];
        final int kFactor = dimExtents[0] * dimExtents[1];
        final int tFactor = dimExtents[0] * dimExtents[1] * dimExtents[2];

        int buffFactor = 1;
        
        Vector3f center = new Vector3f();
        for ( int i = 0; i < verts.length; i++ )
        {
        	center.add(verts[i]);
        }
        center.scale( 1f/verts.length );
        
        /* Calculate the slopes for traversing the data in x,y,z: */
        float xSlopeX = verts[1].X - verts[0].X;
        float ySlopeX = verts[1].Y - verts[0].Y;
        float zSlopeX = verts[1].Z - verts[0].Z;

        float xSlopeY = verts[3].X - verts[0].X;
        float ySlopeY = verts[3].Y - verts[0].Y;
        float zSlopeY = verts[3].Z - verts[0].Z;

        float x0 = verts[0].X;
        float y0 = verts[0].Y;
        float z0 = verts[0].Z;

        xSlopeX /= (iBound);
        ySlopeX /= (iBound);
        zSlopeX /= (iBound);

        xSlopeY /= (jBound);
        ySlopeY /= (jBound);
        zSlopeY /= (jBound);

        /* loop over the 2D image (values) we're writing into */
        float x = x0;
        float y = y0;
        float z = z0;
	

        Vector3f currentPoint = new Vector3f();
        for (int j = 0; j < jBound; j++) {

            /* Initialize the first diagonal point(x,y,z): */
            x = x0;
            y = y0;
            z = z0;

            for (int i = 0; i < iBound; i++) {
                final int iIndex = Math.round(x);
                final int jIndex = Math.round(y);
                final int kIndex = Math.round(z);

                /* calculate the ModelImage space index: */
                final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));

                // Bounds checking:
                if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
                		|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > markerSegmentation.getSize()))) {

                	// do nothing
                } else {
                    currentPoint.set(x, y, z);
            		float currentValue = model.getFloat(iIndex, jIndex, kIndex);
            		float markerValue = markerSegmentation.getFloat(iIndex, jIndex, kIndex);
                    if ( (currentValue == 0) && (markerValue == sliceIDs[slice]) )
                    {
                    	model.set( iIndex, jIndex, kIndex, value );
                    }
                }

                /*
                 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
                 * ySlopeX and zSlopeX values:
                 */
                x = x + xSlopeX;
                y = y + ySlopeX;
                z = z + zSlopeX;
            }

            /*
             * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
             * ySlopeY and zSlopeY values:
             */
            x0 = x0 + xSlopeY;
            y0 = y0 + ySlopeY;
            z0 = z0 + zSlopeY;
        }              
    }


	private void exportDiagonal( ModelImage image, ModelImage model, ModelImage insideConflict,
    		final int tSlice, final int slice, final int[] extents,
            final Vector3f[] verts, final Ellipsoid3f ellipseBound, final float diameter, final Box3f boxBound, final float value) 
    {
        final int iBound = extents[0];
        final int jBound = extents[1];

        int[] dimExtents = image.getExtents();
        
        /*
         * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
         * coordinate-systems: transformation:
         */
        final int iFactor = 1;
        final int jFactor = dimExtents[0];
        final int kFactor = dimExtents[0] * dimExtents[1];
        final int tFactor = dimExtents[0] * dimExtents[1] * dimExtents[2];

        int buffFactor = 1;

        if ( (image.getType() == ModelStorageBase.ARGB) || (image.getType() == ModelStorageBase.ARGB_USHORT)
                || (image.getType() == ModelStorageBase.ARGB_FLOAT)) {
            buffFactor = 4;
        }
        
        Vector3f center = new Vector3f();
        for ( int i = 0; i < verts.length; i++ )
        {
        	center.add(verts[i]);
        }
        center.scale( 1f/verts.length );
        
        /* Calculate the slopes for traversing the data in x,y,z: */
        float xSlopeX = verts[1].X - verts[0].X;
        float ySlopeX = verts[1].Y - verts[0].Y;
        float zSlopeX = verts[1].Z - verts[0].Z;

        float xSlopeY = verts[3].X - verts[0].X;
        float ySlopeY = verts[3].Y - verts[0].Y;
        float zSlopeY = verts[3].Z - verts[0].Z;

        float x0 = verts[0].X;
        float y0 = verts[0].Y;
        float z0 = verts[0].Z;

        xSlopeX /= (iBound);
        ySlopeX /= (iBound);
        zSlopeX /= (iBound);

        xSlopeY /= (jBound);
        ySlopeY /= (jBound);
        zSlopeY /= (jBound);

        /* loop over the 2D image (values) we're writing into */
        float x = x0;
        float y = y0;
        float z = z0;
        
        Vector3f currentPoint = new Vector3f();

        boolean[][] values = new boolean[iBound][jBound];
        for (int j = 0; j < jBound; j++) {

            /* Initialize the first diagonal point(x,y,z): */
            x = x0;
            y = y0;
            z = z0;

            for (int i = 0; i < iBound; i++) {
            	values[i][j] = false;
                final int iIndex = Math.round(x);
                final int jIndex = Math.round(y);
                final int kIndex = Math.round(z);

                /* calculate the ModelImage space index: */
                final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));

                // Bounds checking:
                if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
                		|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > image.getSize()))) {

                	// do nothing
                } else {
                    currentPoint.set(x, y, z);
                    boolean isInside = ellipseBound.Contains(currentPoint);
                    if ( !isInside )
                    {
                    	// do nothing
                    }
                    else
                    {
                    	values[i][j] = true;
                    	for ( int z1 = Math.max(0, (int) Math.floor(z)); z1 <= Math.min(dimExtents[2]-1, Math.ceil(z)); z1++ )
                    	{
                        	for ( int y1 = Math.max(0, (int) Math.floor(y)); y1 <=  Math.min(dimExtents[1]-1, Math.ceil(y)); y1++ )
                        	{
                            	for ( int x1 = Math.max(0, (int) Math.floor(x)); x1 <=  Math.min(dimExtents[0]-1, Math.ceil(x)); x1++ )
                            	{
                                	float currentValue = model.getFloat(x1, y1, z1);
                                	if ( currentValue != 0 )
                                	{
    									if ( Math.abs(currentValue - value) < SampleLimit )
    									{
//    										model.set(x1, y1, z1, (currentValue + value)/2f);
    									}
    									else
    									{
    										insideConflict.set(x1, y1, z1, true);
    									}
                                	}
                                	else
                                	{
                                		model.set(x1, y1, z1, value);
                                	}                            		
                            	}                        		
                        	}
                    	}
                    }
                }

                /*
                 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
                 * ySlopeX and zSlopeX values:
                 */
                x = x + xSlopeX;
                y = y + ySlopeX;
                z = z + zSlopeX;
            }

            /*
             * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
             * ySlopeY and zSlopeY values:
             */
            x0 = x0 + xSlopeY;
            y0 = y0 + ySlopeY;
            z0 = z0 + zSlopeY;
        }

//        int[][] horizontal = new int[jBound][2];
//        int[][] vertical = new int[iBound][2];
//        checkConvex( values, iBound, jBound, iBoundHalf, jBoundHalf, horizontal, vertical );
//        
//        
//
//
//        x0 = verts[0].X;
//        y0 = verts[0].Y;
//        z0 = verts[0].Z;
//
//
//        for (int j = 0; j < jBound; j++) {
//
//            /* Initialize the first diagonal point(x,y,z): */
//            x = x0;
//            y = y0;
//            z = z0;
//
//            for (int i = 0; i < iBound; i++) {
//                final int iIndex = (int) Math.round(x);
//                final int jIndex = (int) Math.round(y);
//                final int kIndex = (int) Math.round(z);
//
//                /* calculate the ModelImage space index: */
//                final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));
//
//                // Bounds checking:
//                if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
//                		|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > image.getSize()))) {
//
//                	// do nothing
//                } else {
//                	if ( (j <= vertical[i][0]) || (j >= vertical[i][1]) || (i <= horizontal[j][0]) || (i >= horizontal[j][1]) )
//                	{
//                    	for ( int z1 = Math.max(0, (int) Math.floor(z)); z1 <= Math.min(dimExtents[2]-1, Math.ceil(z)); z1++ )
//                    	{
//                        	for ( int y1 = Math.max(0, (int) Math.floor(y)); y1 <=  Math.min(dimExtents[1]-1, Math.ceil(y)); y1++ )
//                        	{
//                            	for ( int x1 = Math.max(0, (int) Math.floor(x)); x1 <=  Math.min(dimExtents[0]-1, Math.ceil(x)); x1++ )
//                            	{
//                                	float currentValue = model.getFloat(x1, y1, z1);
//                                	if ( currentValue == value )
//                                	{
//                                		model.set(x1, y1, z1, 0);
//                                	}                            		
//                            	}                        		
//                        	}
//                    	}
//                	}
//                }
//
//                /*
//                 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
//                 * ySlopeX and zSlopeX values:
//                 */
//                x = x + xSlopeX;
//                y = y + ySlopeX;
//                z = z + zSlopeX;
//            }
//
//            /*
//             * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
//             * ySlopeY and zSlopeY values:
//             */
//            x0 = x0 + xSlopeY;
//            y0 = y0 + ySlopeY;
//            z0 = z0 + zSlopeY;
//        }
        
        
    }
	
//    public ModelImage straighten( ModelImage image, VOI samplingPlanes, Vector<Ellipsoid3f> ellipseBounds,
//    		int diameter, int[] latticeSlice, boolean saveStats )
//    {
//    	
//		int colorFactor = image.isColorImage() ? 4 : 1;
//		int[] resultExtents = new int[]{diameter, diameter, samplingPlanes.getCurves().size()};
//		float[][] values = new float[resultExtents[2]][resultExtents[0] * resultExtents[1] * colorFactor]; 
//		float[][] dataOrigin = new float[resultExtents[2]][resultExtents[0] * resultExtents[1] * 4]; 
//
//    	String imageName = image.getImageName();
//    	if ( imageName.contains("_clone") )
//    	{
//    		imageName = imageName.replaceAll("_clone", "" );
//    	}
//		ModelImage resultImage = new ModelImage(image.getType(), resultExtents, imageName + "_straight.xml");
//		JDialogBase.updateFileInfo( image, resultImage );
//		resultImage.setResolutions( new float[]{1,1,1});
//		
//		ModelImage straightToOrigin = new ModelImage( ModelStorageBase.ARGB_FLOAT, resultExtents, imageName + "_toOriginal.xml");
//		JDialogBase.updateFileInfo( image, straightToOrigin );
//		straightToOrigin.setResolutions( new float[]{1,1,1});
//		for ( int i = 0; i < straightToOrigin.getDataSize(); i++ )
//		{
//			straightToOrigin.set(i, 0);
//		}
//		
//		Vector3f lpsOrigin = new Vector3f();
//		for( int i = 0; i < samplingPlanes.getCurves().size(); i++ )
//		{
//			VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
//	        Vector3f[] corners = new Vector3f[4];
//	        for ( int j = 0; j < 4; j++ )
//	        {
//	        	corners[j] = kBox.elementAt(j);
//	        }
//			try {
//				image.exportDiagonal( 0, i, resultExtents, corners, 
//						ellipseBounds.elementAt(i), values[i], true, dataOrigin[i]);
//
//				if ( i == 0 )
//				{
//					MipavCoordinateSystems.fileToScanner( corners[0], lpsOrigin, image );
//				}
//
//				resultImage.importData(i*values[i].length, values[i], false);
//				straightToOrigin.importData(i*dataOrigin[i].length, dataOrigin[i], false);
//			} catch(IOException e) {
//				e.printStackTrace();
//			}
//		}
//		
//
//		float[] leftDistances = new float[latticeSlice.length];
//		float[] rightDistances = new float[latticeSlice.length];
//		short id = (short) image.getVOIs().getUniqueID();
//		VOI lattice = new VOI(id, "lattice", VOI.POLYLINE, (float)Math.random() );
//		VOIContour leftSide = new VOIContour( false );
//		VOIContour rightSide = new VOIContour( false );
//		lattice.getCurves().add(leftSide);		
//		lattice.getCurves().add(rightSide);
//		Vector3f dir = new Vector3f(1,0,0);
//		for ( int i = 0; i < latticeSlice.length; i++ )
//		{
//			Ellipsoid3f ellipsoid = ellipseBounds.elementAt( latticeSlice[i] );
////			Vector3f center = ellipsoid.Center;
//			float width = ellipsoid.Extent[0] - DiameterBuffer;
////			Vector3f dir = ellipsoid.Axis[0];
//			Vector3f center = new Vector3f(diameter/2,diameter/2,latticeSlice[i]);
//			
//			Vector3f leftPt = Vector3f.scale( -width, dir ); leftPt.add(center);
//			leftSide.add(leftPt);
//			
//			Vector3f rightPt = Vector3f.scale(  width, dir ); rightPt.add(center);
//			rightSide.add(rightPt);
//
//			leftDistances[i] = 0;
//			rightDistances[i] = 0;
//			if ( i > 0 )
//			{
//				leftDistances[i] = leftSide.elementAt(i).distance(leftSide.elementAt(i-1) );
//				rightDistances[i] = rightSide.elementAt(i).distance(rightSide.elementAt(i-1) );
//			}
//		}
//
//		resultImage.registerVOI(lattice);
//
//		lattice.setColor( new Color( 0, 0, 255) );
//		lattice.getCurves().elementAt(0).update( new ColorRGBA(0,0,1,1));
//		lattice.getCurves().elementAt(1).update( new ColorRGBA(0,0,1,1));
//		lattice.getCurves().elementAt(0).setClosed(false);
//		lattice.getCurves().elementAt(1).setClosed(false);
//		for ( int j = 0; j < leftSide.size(); j++ )
//		{
//			id = (short) image.getVOIs().getUniqueID();
//			VOI marker = new VOI(id, "pair_" + j, VOI.POLYLINE, (float)Math.random() );
//			VOIContour mainAxis = new VOIContour(false); 		    		    		
//			mainAxis.add( leftSide.elementAt(j) );
//			mainAxis.add( rightSide.elementAt(j) );
//			marker.getCurves().add(mainAxis);
//			marker.setColor( new Color( 255, 255, 0) );
//			mainAxis.update( new ColorRGBA(1,1,0,1));
//			if ( j == 0 )
//			{
//				marker.setColor( new Color( 0, 255, 0) );
//				mainAxis.update( new ColorRGBA(0,1,0,1));
//			}
//			resultImage.registerVOI( marker );
//		}
//		resultImage.calcMinMax();
//		new ViewJFrameImage(resultImage);  	
//		saveTransformImage(imageName, resultImage, true);
//
//		if ( saveStats )
//		{ 
//			
//			saveLatticeStatistics(image, resultExtents[2], leftSide, rightSide, leftDistances, rightDistances, "_after");			
//			saveTransformImage(imageName, straightToOrigin, false);
//			ModelImage originToStraight = computeOriginToStraight(image, straightToOrigin);
//			saveTransformImage(imageName, originToStraight, false);
//			
//			ModelImage croppedVolume = computeMissingData(originToStraight);
//			saveTransformImage(imageName, croppedVolume, false);
//
////			testTransform( resultImage, straightToOrigin, image.getExtents() );
////			testTransform( image, originToStraight, resultImage.getExtents() );
//		}
//		
//		
//		
//		return resultImage;
//    }
    

    private void generateCurves( )
    {
    	clearCurves();
    	short sID;
		
		center = new VOIContour(false);
		for ( int i = 0; i < left.size(); i++ )
		{
			Vector3f centerPt = Vector3f.add(left.elementAt(i), right.elementAt(i) );
			centerPt.scale(0.5f);
			center.add(centerPt);
		}
    	afTimeC = new float[center.size()];
    	centerSpline = smoothCurve(center, afTimeC);
		leftSpline = smoothCurve2(left, afTimeC);
		rightSpline = smoothCurve2(right, afTimeC);

		centerPositions = new VOIContour(false);
		leftPositions = new VOIContour(false);
		rightPositions = new VOIContour(false);

	    wormDiameters = new Vector<Float>();
	    rightVectors = new Vector<Vector3f>();
	    upVectors = new Vector<Vector3f>();
	    
		
		length = centerSpline.GetLength(0, 1);
		allTimes = new float[(int) (Math.ceil(length)) +1];
		extent = -1;
		for ( int i = 0; i <= length; i++ )
		{
			float t = centerSpline.GetTime(i);
			centerPositions.add(centerSpline.GetPosition(t));
			leftPositions.add(leftSpline.GetPosition(t));
			rightPositions.add(rightSpline.GetPosition(t));
			
			
			allTimes[i] = t;
			Vector3f normal = centerSpline.GetTangent(t);
			Vector3f leftPt = leftSpline.GetPosition(t);
			Vector3f rightPt = rightSpline.GetPosition(t);
			
			Vector3f rightDir = Vector3f.sub( rightPt, leftPt );		
			float diameter = rightDir.normalize();
			diameter /= 2f;
			diameter += DiameterBuffer;
			if ( diameter > extent )
			{
				extent = (int) Math.ceil(diameter);
			}			
			wormDiameters.add(diameter);
			rightVectors.add(rightDir);
			
			Vector3f upDir = Vector3f.cross( normal, rightDir );
			upDir.normalize();
			upVectors.add(upDir);
		}
		extent += 10;
		
		
		sID = (short)(imageA.getVOIs().getUniqueID());
		displayContours = new VOI(sID, "wormContours");
		for ( int i = 0; i < centerPositions.size(); i += 30 )
		{
	        Vector3f rkEye = centerPositions.elementAt(i);
	        Vector3f rkRVector = rightVectors.elementAt(i);
	        Vector3f rkUVector = upVectors.elementAt(i);
	        
	        float curve = centerSpline.GetCurvature(allTimes[i]);
	        float scale = curve;//(curve - minCurve)/(maxCurve - minCurve);
//	        System.err.println( scale );
	        VOIContour ellipse = new VOIContour(true);
	        ellipse.setVolumeDisplayRange(minRange);
	        makeEllipse2( rkRVector, rkUVector, rkEye, wormDiameters.elementAt(i), scale, ellipse );
	        displayContours.getCurves().add(ellipse);
		}
		
		sID = (short)(imageA.getVOIs().getUniqueID());
		centerLine = new VOI(sID, "center line");
		centerLine.getCurves().add(centerPositions);
		centerLine.setColor( Color.red );
		centerPositions.update( new ColorRGBA(1,0,0,1));
//		centerPositions.setVolumeDisplayRange(minRange);

		sID++;
		leftLine = new VOI(sID, "left line");
		leftLine.getCurves().add(leftPositions);
		leftLine.setColor( Color.magenta );
		leftPositions.update( new ColorRGBA(1,0,1,1));
//		leftPositions.setVolumeDisplayRange(minRange);

		sID++;
		rightLine = new VOI(sID, "right line");
		rightLine.getCurves().add(rightPositions);
		rightLine.setColor( Color.green );
		rightPositions.update( new ColorRGBA(0,1,0,1));
//		rightPositions.setVolumeDisplayRange(minRange);
			

		imageA.registerVOI(leftLine);
		imageA.registerVOI(rightLine);
		imageA.registerVOI(centerLine);
    }
    

    private void clearCurves( )
    {

    	if ( center != null )
    	{
    		center.dispose();
    		center = null;
    	}
		afTimeC = null;
		centerSpline = null;
		leftSpline = null;
		rightSpline = null;

		centerPositions = null;
		leftPositions = null;
		rightPositions = null;

		if ( wormDiameters != null )
		{
			wormDiameters.removeAllElements();
			wormDiameters = null;
		}
		if ( rightVectors != null )
		{
			rightVectors.removeAllElements();
			rightVectors = null;
		}
		if ( upVectors != null )
		{
			upVectors.removeAllElements();
			upVectors = null;
		}
	    
		allTimes = null;
		

		if ( centerLine != null )
		{
			imageA.unregisterVOI( centerLine );
			centerLine.dispose();
			centerLine = null;
		}
		if ( rightLine != null )
		{
			imageA.unregisterVOI( rightLine );
			rightLine.dispose();
			rightLine = null;
		}
		if ( leftLine != null )
		{
			imageA.unregisterVOI( leftLine );
			leftLine.dispose();
			leftLine = null;
		}
		
		if ( displayContours != null )
		{
			imageA.unregisterVOI( displayContours );
			displayContours.dispose();
			displayContours = null;
		}
    }

    private boolean checkAnnotations( ModelImage model )
    {
    	boolean outsideFound = false;
    	for ( int i = 0; i < left.size(); i++ )
    	{
        	Vector3f position = left.elementAt(i);
        	int x = Math.round(position.X);
        	int y = Math.round(position.Y);
        	int z = Math.round(position.Z);
    		float value = model.getFloat( x, y, z );
//    		float value = model.getFloatTriLinearBounds( position.X, position.Y, position.Z );
    		if ( value == 0 )
    		{
    			outsideFound = true;
    		}
        	position = right.elementAt(i);
        	x = Math.round(position.X);
        	y = Math.round(position.Y);
        	z = Math.round(position.Z);
    		value = model.getFloat( x, y, z );
//    		value = model.getFloatTriLinearBounds( position.X, position.Y, position.Z );
    		if ( value == 0 )
    		{
    			outsideFound = true;
    		}
    	}
    	
    	if ( annotationVOIs == null )
    	{
        	return !outsideFound;
//        	return true;
    	}
    	if ( annotationVOIs.getCurves().size() == 0 )
    	{
        	return !outsideFound;
//        	return true;
    	}
    	
//    	outsideFound = false;
    	for ( int i = 0; i < annotationVOIs.getCurves().size(); i++ )
    	{
        	VOIText text = (VOIText) annotationVOIs.getCurves().elementAt(i);
        	Vector3f position = text.elementAt(0);
        	int x = Math.round(position.X);
        	int y = Math.round(position.Y);
        	int z = Math.round(position.Z);
    		float value = model.getFloat( x, y, z );
//    		float value = model.getFloatTriLinearBounds( position.X, position.Y, position.Z );
//    		System.err.println( text.getText() + " " + position + "  " + value );
    		if ( value == 0 )
    		{
    			outsideFound = true;
    		}
    	}
    	return !outsideFound;
    }
    
    
    private void generateMasks( ModelImage imageA, ModelImage imageB, VOI samplingPlanes, 
    		Vector<Ellipsoid3f> ellipseBounds, Vector<Float> diameters, int diameter, boolean straighten, boolean displayResult  )
    {
		int[] resultExtents = new int[]{diameter, diameter, samplingPlanes.getCurves().size()};
		
    	String imageName = imageA.getImageName();
    	if ( imageName.contains("_clone") )
    	{
    		imageName = imageName.replaceAll("_clone", "" );
    	}
		ModelImage model = new ModelImage(ModelStorageBase.FLOAT, imageA.getExtents(), imageName + "_model.xml");
		JDialogBase.updateFileInfo( imageA, model );		
		
		ModelImage insideConflict = new ModelImage(ModelStorageBase.BOOLEAN, imageA.getExtents(), imageName + "_insideConflict.xml");
		JDialogBase.updateFileInfo( imageA, insideConflict );		
		
		ModelImage inside = new ModelImage(ModelStorageBase.INTEGER, imageA.getExtents(), imageName + "_insideMask.xml");
		JDialogBase.updateFileInfo( imageA, inside );		
		
		
    	int dimX = imageA.getExtents().length > 0 ? imageA.getExtents()[0] : 1;
    	int dimY = imageA.getExtents().length > 1 ? imageA.getExtents()[1] : 1;
    	int dimZ = imageA.getExtents().length > 2 ? imageA.getExtents()[2] : 1;
    	for ( int z = 0; z < dimZ; z++ )
    	{
    		for ( int y = 0; y < dimY; y++ )
    		{
    			for ( int x = 0; x < dimX; x++ )
    			{
    				model.set(x, y, z, 0);
    				insideConflict.set(x,y,z,false);
    			}
    		}
    	}


    	for ( int i = 0; i < samplingPlanes.getCurves().size(); i++ )
    	{
    		VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
    		Vector3f[] corners = new Vector3f[4];
    		for ( int j = 0; j < 4; j++ )
    		{
    			corners[j] = kBox.elementAt(j);
    		}

    		float planeDist = -Float.MAX_VALUE;
    		if ( i < (samplingPlanes.getCurves().size() - 1) )
    		{
    			kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i+1);
    			for ( int j = 0; j < 4; j++ )
    			{
    				float distance = corners[j].distance(kBox.elementAt(j));
    				if ( distance > planeDist )
    				{
    					planeDist = distance;
    				}
    			}	        	
    		}

    		if ( i < (samplingPlanes.getCurves().size() - 1) )
    		{
    			planeDist *= 3;
    			kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i+1);
    			Vector3f[] steps = new Vector3f[4];
    			Vector3f[] cornersSub = new Vector3f[4];
    			for ( int j = 0; j < 4; j++ )
    			{
    				steps[j] = Vector3f.sub( kBox.elementAt(j), corners[j] ); steps[j].scale(1f/planeDist);
    				cornersSub[j] = new Vector3f(corners[j]);
    			}
    			for ( int j = 0; j < planeDist; j++ )
    			{
    				exportDiagonal( imageA, model, insideConflict, 0, i, resultExtents, cornersSub, 
    						ellipseBounds.elementAt(i), 1.5f*diameters.elementAt(i), boxBounds.elementAt(i), i+1);
    				for ( int k = 0; k < 4; k++ )
    				{
    					cornersSub[k].add(steps[k]);
    				}
    			}
    		}
    		else
    		{
    			planeDist = 15;
    			kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i-1);
    			Vector3f[] steps = new Vector3f[4];
    			Vector3f[] cornersSub = new Vector3f[4];
    			for ( int j = 0; j < 4; j++ )
    			{
    				steps[j] = Vector3f.sub( corners[j], kBox.elementAt(j) ); steps[j].scale(1f/planeDist);
    				//				        cornersSub[j] = Vector3f.add( corners[j], kBox.elementAt(j) ); cornersSub[j].scale(0.5f);
    				cornersSub[j] = new Vector3f(corners[j]);
    			}
    			for ( int j = 0; j < planeDist; j++ )
    			{
    				exportDiagonal( imageA, model, insideConflict, 0, i, resultExtents, cornersSub, 
    						ellipseBounds.elementAt(i), 1.5f*diameters.elementAt(i), boxBounds.elementAt(i), i+1);
    				for ( int k = 0; k < 4; k++ )
    				{
    					cornersSub[k].add(steps[k]);
    				}
    			}		        	
    		}				
    	}

    	for ( int z = 0; z < dimZ; z++ )
    	{
    		for ( int y = 0; y < dimY; y++ )
    		{
    			for ( int x = 0; x < dimX; x++ )
    			{
    				if ( insideConflict.getBoolean(x,y,z) )
    				{
    					model.set(x, y, z, 0);
    				}
    			}
    		}
    	}

//    	model.calcMinMax();
//		new ViewJFrameImage((ModelImage)model.clone());
//
//		Vector3f currentPoint = new Vector3f();
//    	for ( int z = 0; z < dimZ; z++ )
//    	{
//    		for ( int y = 0; y < dimY; y++ )
//    		{
//    			for ( int x = 0; x < dimX; x++ )
//    			{
//    				insideConflict.set(x, y, z, false);
//    				model.set(x, y, z, 0);
//    				
//    				for ( int i = 0; i < ellipseBounds.size(); i++ )
//    				{
//    					currentPoint.set(x, y, z);
//    					Box3f box = boxBounds.elementAt(i);
//    					if ( ContBox3f.InBox( currentPoint, box ) )
//    					{
////    						Ellipsoid3f ellipseBound = ellipseBounds.elementAt(i);
////    						boolean isInside = ellipseBound.Contains(currentPoint);
////    						if ( isInside )
//    						{
//    							float currentValue = model.getFloat(x, y, z);
//    							if ( currentValue == 0 )
//    							{
//    								model.set(x, y, z, i+1);
//    							}
//    							else if ( Math.abs(currentValue - (i+1)) >= SampleLimit )
//    							{
//    								insideConflict.set(x, y, z, true );
//    								break;
//    							}
//    						}
//    					}
//    				}
//    				
//    				if ( insideConflict.getBoolean(x,y,z) )
//    				{
//    					model.set(x, y, z, 0);
//    				}
//    			}
//    		}
//    	}
		
		
    	insideConflict.disposeLocal();
    	insideConflict = null;

		
		

//		if ( growContours != null )
//		{
//			growContours.dispose();
//			growContours = null;
//		}
//    	short sID = (short)(imageA.getVOIs().getUniqueID());
//		growContours = new VOI(sID, "growContours");
//
//		for ( int i = 0; i < centerPositions.size(); i++ )
//		{
//			int value = i+1;
//			
//			Vector3f rkEye = centerPositions.elementAt(i);
//			Vector3f rkRVector = rightVectors.elementAt(i);
//			Vector3f rkUVector = upVectors.elementAt(i);
//
//	        float curve = centerSpline.GetCurvature(allTimes[i]);
//	        float scale = curve;
//			VOIContour ellipse = new VOIContour(true);
//			ellipse.setVolumeDisplayRange(minRange);
//			makeEllipse2( rkRVector, rkUVector, rkEye, wormDiameters.elementAt(i), scale, ellipse );
//
//			interpolateContour( ellipse );
//			for ( int j = 0; j < ellipse.size(); j++ )
//			{
//				Vector3f pt = ellipse.elementAt(j);
//				Vector3f diff = Vector3f.sub( pt, centerPositions.elementAt(i) );
//				diff.normalize();
//				
//				pt.copy( centerPositions.elementAt(i) );
//				int x = Math.round(pt.X);
//				int y = Math.round(pt.Y);
//				int z = Math.round(pt.Z);
//				float currentValue = model.getFloat(x, y, z);
//				while ( ((currentValue != 0) && Math.abs(currentValue - value) <= SampleLimit) )
//				{
//					pt.add(diff);
//    				x = Math.round(pt.X);
//    				y = Math.round(pt.Y);
//    				z = Math.round(pt.Z);
//					currentValue = model.getFloat(x, y, z);				
//				}
//				if ( !pt.isEqual( centerPositions.elementAt(i) ) )
//				{
//					pt.sub(diff);
//				}
//			}
//			growContours.getCurves().add(ellipse);
//		}
//
//    	model.calcMinMax();
//    	model.registerVOI( growContours );
//		new ViewJFrameImage((ModelImage)model.clone());
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
//    	if ( model != null )
//    	{
//    		return;
//    	}
    	

//    	int[][] markerVolumes = new int[left.size()][2];
//    	int[] markerIDs = new int[left.size()];
//    	boolean[] completedIDs = new boolean[left.size()];
//    	int[] currentID = new int[]{0};
//    	ModelImage markerSegmentation = segmentMarkers( imageA, left, right, markerIDs, markerVolumes );
//		for ( int i = 0; i < completedIDs.length; i++ )
//		{
//			if ( markerIDs[i] == 0 )
//			{
//				completedIDs[i] = true;
//			}
//		}
		saveTransformImage(imageName, markerSegmentation, true);
		if ( displayResult )
		{
			markerSegmentation.calcMinMax();
			new ViewJFrameImage((ModelImage)markerSegmentation.clone());
		}
		
		float[] sliceIDs = new float[samplingPlanes.getCurves().size()];
		for ( int i = 0; i < samplingPlanes.getCurves().size(); i++ )
		{
			sliceIDs[i] = 0;
			VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
			Vector3f[] corners = new Vector3f[4];
			for ( int j = 0; j < 4; j++ )
			{
				corners[j] = kBox.elementAt(j);
			}
			exportDiagonal( model, markerSegmentation, sliceIDs, markerIDs, completedIDs, currentID, 0, i,  resultExtents, corners, ellipseBounds.elementAt(i) );
		}
		for ( int i = 0; i < samplingPlanes.getCurves().size(); i++ )
		{
//			if ( sliceIDs[i] != 0 )
//			{
////				System.err.println( i + " " + sliceIDs[i] );
//				VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
//				Vector3f[] corners = new Vector3f[4];
//				for ( int j = 0; j < 4; j++ )
//				{
//					corners[j] = kBox.elementAt(j);
//				}
//				exportDiagonal( model, markerSegmentation, sliceIDs, 0, i,  resultExtents, corners, ellipseBounds.elementAt(i) );
//			}
			
			

    		VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
    		Vector3f[] corners = new Vector3f[4];
    		for ( int j = 0; j < 4; j++ )
    		{
    			corners[j] = kBox.elementAt(j);
    		}

    		float planeDist = -Float.MAX_VALUE;
    		if ( i < (samplingPlanes.getCurves().size() - 1) )
    		{
    			kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i+1);
    			for ( int j = 0; j < 4; j++ )
    			{
    				float distance = corners[j].distance(kBox.elementAt(j));
    				if ( distance > planeDist )
    				{
    					planeDist = distance;
    				}
    			}	        	
    		}

    		if ( sliceIDs[i] != 0 )
    		{
    			if ( i < (samplingPlanes.getCurves().size() - 1) )
    			{
    				planeDist *= 3;
    				kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i+1);
    				Vector3f[] steps = new Vector3f[4];
    				Vector3f[] cornersSub = new Vector3f[4];
    				for ( int j = 0; j < 4; j++ )
    				{
    					steps[j] = Vector3f.sub( kBox.elementAt(j), corners[j] ); steps[j].scale(1f/planeDist);
    					cornersSub[j] = new Vector3f(corners[j]);
    				}
    				for ( int j = 0; j < planeDist; j++ )
    				{
    					exportDiagonal( model, markerSegmentation, sliceIDs, 0, i,  resultExtents, cornersSub, ellipseBounds.elementAt(i) );
//    					exportDiagonal( model, markerSegmentation, sliceIDs, 0, i,  resultExtents, cornersSub, i+1 );
    					for ( int k = 0; k < 4; k++ )
    					{
    						cornersSub[k].add(steps[k]);
    					}
    				}
    			}
    			else
    			{
    				planeDist = 15;
    				kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i-1);
    				Vector3f[] steps = new Vector3f[4];
    				Vector3f[] cornersSub = new Vector3f[4];
    				for ( int j = 0; j < 4; j++ )
    				{
    					steps[j] = Vector3f.sub( corners[j], kBox.elementAt(j) ); steps[j].scale(1f/planeDist);
    					//				        cornersSub[j] = Vector3f.add( corners[j], kBox.elementAt(j) ); cornersSub[j].scale(0.5f);
    					cornersSub[j] = new Vector3f(corners[j]);
    				}
    				for ( int j = 0; j < planeDist; j++ )
    				{
    					exportDiagonal( model, markerSegmentation, sliceIDs, 0, i,  resultExtents, cornersSub, ellipseBounds.elementAt(i) );
//    					exportDiagonal( model, markerSegmentation, sliceIDs, 0, i,  resultExtents, cornersSub, i+1 );
    					for ( int k = 0; k < 4; k++ )
    					{
    						cornersSub[k].add(steps[k]);
    					}
    				}		        	
    			}				
    		}
		}
		
		if ( displayResult )
		{
			markerSegmentation.calcMinMax();
			new ViewJFrameImage((ModelImage)markerSegmentation.clone());
		}
//		if ( markerSegmentation != null )
//			return;
		
		
		


    	for ( int i = 0; i < samplingPlanes.getCurves().size(); i++ )
    	{
    		VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
    		Vector3f[] corners = new Vector3f[4];
    		for ( int j = 0; j < 4; j++ )
    		{
    			corners[j] = kBox.elementAt(j);
    		}

    		float planeDist = -Float.MAX_VALUE;
    		if ( i < (samplingPlanes.getCurves().size() - 1) )
    		{
    			kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i+1);
    			for ( int j = 0; j < 4; j++ )
    			{
    				float distance = corners[j].distance(kBox.elementAt(j));
    				if ( distance > planeDist )
    				{
    					planeDist = distance;
    				}
    			}	        	
    		}

    		if ( sliceIDs[i] != 0 )
    		{
    			if ( i < (samplingPlanes.getCurves().size() - 1) )
    			{
    				planeDist *= 3;
    				kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i+1);
    				Vector3f[] steps = new Vector3f[4];
    				Vector3f[] cornersSub = new Vector3f[4];
    				for ( int j = 0; j < 4; j++ )
    				{
    					steps[j] = Vector3f.sub( kBox.elementAt(j), corners[j] ); steps[j].scale(1f/planeDist);
    					cornersSub[j] = new Vector3f(corners[j]);
    				}
    				for ( int j = 0; j < planeDist; j++ )
    				{
    					exportDiagonal( model, markerSegmentation, sliceIDs, 0, i,  resultExtents, cornersSub, i+1 );
    					for ( int k = 0; k < 4; k++ )
    					{
    						cornersSub[k].add(steps[k]);
    					}
    				}
    			}
    			else
    			{
    				planeDist = 15;
    				kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i-1);
    				Vector3f[] steps = new Vector3f[4];
    				Vector3f[] cornersSub = new Vector3f[4];
    				for ( int j = 0; j < 4; j++ )
    				{
    					steps[j] = Vector3f.sub( corners[j], kBox.elementAt(j) ); steps[j].scale(1f/planeDist);
    					//				        cornersSub[j] = Vector3f.add( corners[j], kBox.elementAt(j) ); cornersSub[j].scale(0.5f);
    					cornersSub[j] = new Vector3f(corners[j]);
    				}
    				for ( int j = 0; j < planeDist; j++ )
    				{
    					exportDiagonal( model, markerSegmentation, sliceIDs, 0, i,  resultExtents, cornersSub, i+1 );
    					for ( int k = 0; k < 4; k++ )
    					{
    						cornersSub[k].add(steps[k]);
    					}
    				}		        	
    			}				
    		}
    	}

		
		
		
		
		

//		System.err.println( "    generateMasks check annotations " + checkAnnotations(model) );
//		model.calcMinMax();
//		new ViewJFrameImage((ModelImage)model.clone());		
		
		
    	int growStep = 0;
    	while ( (growStep < 25) && (!checkAnnotations(model) || (growStep < 20) ) )
    	{		
    		growEdges( model, markerSegmentation, sliceIDs, growStep++ );
//    		growEdges( model, resultExtents, growStep++ );
    	}
    	if ( !checkAnnotations(model) )
    	{
    		System.err.println( "    generateMasks " + growStep + " " + false );
    	}
		if ( !straighten )
		{
	    	short sID = (short)(imageA.getVOIs().getUniqueID());
	    	if ( displayInterpolatedContours != null )
	    	{
				imageA.unregisterVOI(displayInterpolatedContours);
				displayInterpolatedContours.dispose();
				displayInterpolatedContours = null;
	    	}
	    	displayInterpolatedContours = new VOI(sID, "interpolatedContours");
	    	displayInterpolatedContours.setColor( Color.blue );
	    	
			for ( int i = 0; i < growContours.getCurves().size(); i += 10 )
			{
				VOIContour contour = (VOIContour) growContours.getCurves().elementAt(i).clone();
				contour.trimPoints(0.5, true);
				displayInterpolatedContours.getCurves().add(contour);
				contour.update( new ColorRGBA(0,1,0,1));
				contour.setVolumeDisplayRange(minRange);
			}
			imageA.registerVOI(displayInterpolatedContours);	
	        imageA.notifyImageDisplayListeners();		
		}
//		
//		model.calcMinMax();
//		model.registerVOI( displayInterpolatedContours );
//		new ViewJFrameImage((ModelImage)model.clone());		
////		modelToStraight( imageA, model, resultExtents, imageName, true, displayResult, true );
		
		
		if ( straighten )
		{
			for ( int z = 0; z < dimZ; z++ )
			{
				for ( int y = 0; y < dimY; y++ )
				{
					for ( int x = 0; x < dimX; x++ )
					{
						if ( model.getFloat(x,y,z) != 0 )
						{
							inside.set(x, y, z, 1);
						}
					}
				}
			}
			saveTransformImage(imageName, inside, false);
			
			
			straighten(imageA, resultExtents, imageName, model, true, displayResult, true );
			if ( imageB != null )
			{
				straighten(imageB, resultExtents, imageName, model, false, displayResult, true );			
			}
		}
		
		markerSegmentation.disposeLocal();
		markerSegmentation = null;
//		if ( displayResult )
//		{
//			inside.calcMinMax();
//			new ViewJFrameImage(inside);
//		}
//		else
		{
			inside.disposeLocal();
			inside = null;
		}
		model.disposeLocal();
		model = null;
    }

    
    private void growEdges( ModelImage model, ModelImage markers, float[] sliceIDs, int step )
    {
    	int dimX = model.getExtents().length > 0 ? model.getExtents()[0] : 1;
    	int dimY = model.getExtents().length > 1 ? model.getExtents()[1] : 1;
    	int dimZ = model.getExtents().length > 2 ? model.getExtents()[2] : 1;    	

    	if ( step == 0 )
    	{
    		if ( growContours != null )
    		{
    			growContours.dispose();
    			growContours = null;
    		}
        	short sID = (short)(imageA.getVOIs().getUniqueID());
    		growContours = new VOI(sID, "growContours");

    		for ( int i = 0; i < centerPositions.size(); i++ )
    		{
    			int value = i+1;
    			
    			Vector3f rkEye = centerPositions.elementAt(i);
    			Vector3f rkRVector = rightVectors.elementAt(i);
    			Vector3f rkUVector = upVectors.elementAt(i);

    	        float curve = centerSpline.GetCurvature(allTimes[i]);
    	        float scale = curve;
    			VOIContour ellipse = new VOIContour(true);
    			ellipse.setVolumeDisplayRange(minRange);
    			makeEllipse2( rkRVector, rkUVector, rkEye, wormDiameters.elementAt(i), scale, ellipse );

    			interpolateContour( ellipse );
    			for ( int j = 0; j < ellipse.size(); j++ )
    			{
    				Vector3f start = new Vector3f( ellipse.elementAt(j) );
    				Vector3f pt = ellipse.elementAt(j);
					Vector3f diff = Vector3f.sub( pt, centerPositions.elementAt(i) );
					diff.normalize();
					
					pt.copy( centerPositions.elementAt(i) );
    				int x = Math.round(pt.X);
    				int y = Math.round(pt.Y);
    				int z = Math.round(pt.Z);
					float currentValue = model.getFloat(x, y, z);
					while ( ((currentValue != 0) && Math.abs(currentValue - value) <= SampleLimit) )
					{
						pt.add(diff);
	    				x = Math.round(pt.X);
	    				y = Math.round(pt.Y);
	    				z = Math.round(pt.Z);
	    				if ( (x < 0) || (x >= dimX) || (y < 0) || (y >= dimY) || (z < 0) || (z >= dimZ) )
	    				{
	    					break;
	    				}
						currentValue = model.getFloat(x, y, z);				
					}
					if ( !pt.isEqual( centerPositions.elementAt(i) ) )
					{
						pt.sub(diff);
					}
					float distStart = start.distance( centerPositions.elementAt(i) );
					float distPt = pt.distance( centerPositions.elementAt(i) );
					if ( distStart > distPt )
					{
	    				x = Math.round(start.X);
	    				y = Math.round(start.Y);
	    				z = Math.round(start.Z);
	    				if ( !((x < 0) || (x >= dimX) || (y < 0) || (y >= dimY) || (z < 0) || (z >= dimZ)) )
	    				{
	    					currentValue = model.getFloat(x, y, z);				
	    					if ( ((currentValue != 0) && Math.abs(currentValue - value) <= SampleLimit) )
	    					{
	    						diff = Vector3f.sub( start, pt );
	    						diff.normalize();
	    						while ( !pt.isEqual( start ) && (distPt < distStart) )
	    						{
	    							pt.add(diff);
	    							x = Math.round(pt.X);
	    							y = Math.round(pt.Y);
	    							z = Math.round(pt.Z);
	    							if ( (x < 0) || (x >= dimX) || (y < 0) || (y >= dimY) || (z < 0) || (z >= dimZ) )
	    							{
	    								break;
	    							}
	    							model.set(x, y, z, currentValue);
	    							distPt = pt.distance( centerPositions.elementAt(i) );
	    						}							
	    					}
	    				}
					}
    			}
    			growContours.getCurves().add(ellipse);
    		}
//    		return;
    	}
    	
		for ( int i = 0; i < centerPositions.size(); i++ )
		{
			int value = i+1;
			VOIContour ellipse = (VOIContour) growContours.getCurves().elementAt(i);
			interpolateContour( ellipse );
			for ( int j = 0; j < ellipse.size(); j++ )
			{
				Vector3f pt = ellipse.elementAt(j);
				Vector3f diff = Vector3f.sub( pt, centerPositions.elementAt(i) );
				float distance = diff.normalize();
//				diff.scale(0.5f);

    			float x = pt.X + diff.X;
    			float y = pt.Y + diff.Y;
    			float z = pt.Z + diff.Z;
    			boolean extend = true;
    			for ( int z1 = Math.max(0, (int) Math.floor(z)); (z1 <= Math.min(dimZ-1, Math.ceil(z))) && extend; z1++ )
    			{
    				for ( int y1 = Math.max(0, (int) Math.floor(y)); (y1 <= Math.min(dimY-1, Math.ceil(y))) && extend; y1++ )
    				{
    					for ( int x1 = Math.max(0, (int) Math.floor(x)); (x1 <= Math.min(dimX-1, Math.ceil(x))) && extend; x1++ )
    					{
    						float currentValue = model.getFloat(x1, y1, z1);
                        	if ( currentValue != 0 )
                        	{
                        		if ( Math.abs(currentValue - value) > SampleLimit )
                        		{
                        			extend = false;
                        			break;
                        		}
                        	}
                        	if ( markers != null )
                        	{
                        		float markerValue = markers.getFloat(x1,y1,z1);
                        		if ( (markerValue != 0) && (markerValue != sliceIDs[i]) )
                        		{
                        			extend = false;
                        			break;
                        		}
                        	}
    					}
    				}
    			}
    			if ( extend )
    			{
        			for ( int z1 = Math.max(0, (int) Math.floor(z)); (z1 <= Math.min(dimZ-1, Math.ceil(z))) && extend; z1++ )
        			{
        				for ( int y1 = Math.max(0, (int) Math.floor(y)); (y1 <= Math.min(dimY-1, Math.ceil(y))) && extend; y1++ )
        				{
        					for ( int x1 = Math.max(0, (int) Math.floor(x)); (x1 <= Math.min(dimX-1, Math.ceil(x))) && extend; x1++ )
        					{
        						float currentValue = model.getFloat(x1, y1, z1);
        						if ( currentValue == 0 )
                            	{
                            		model.set(x1, y1, z1, value);
                            	}                            		
        					}
        				}
        			}
    				pt.add(diff);
    			}
			}
		}
    }

    private void interpolateContour( VOIContour contour )
    {
    	int index = 0;
    	while ( index < contour.size() )
    	{
    		Vector3f p1 = contour.elementAt(index);
    		Vector3f p2 = contour.elementAt((index+1)%contour.size());
//    		System.err.println( index + " " + (index+1)%contour.size() );
    		float distance = p1.distance(p2);
    		if ( distance > 1 )
    		{
    			Vector3f dir = Vector3f.sub(p2, p1);
    			dir.normalize();
    			int count = (int)distance;
    			float stepSize = distance / (count+1);
    			float currentStep = stepSize;
    			index++;
    			for ( int i = 0; i < count; i++ )
    			{
    				Vector3f newPt = new Vector3f();
    				newPt.scaleAdd(currentStep, dir, p1);
    				contour.add( index++, newPt );
//    				System.err.println( "    adding pt at " + (index-1) + " " + newPt.distance(p1) + " " + newPt.distance(p2) );
    				currentStep += stepSize;
    			}
    		}
    		else
    		{
    			index++;
    		}
    	}
//    	System.err.println(contour.size());
//    	for ( int i = 0; i < contour.size(); i++ )
//    	{
//    		System.err.println( contour.elementAt(i) + " " + contour.elementAt(i).distance( contour.elementAt((i+1)%contour.size() ) )  );
//    	}
    }
    
    private Ellipsoid3f makeEllipse( Vector3f right, Vector3f up, Vector3f center, float diameterA, float scale, VOIContour ellipse  )
	{
		int numPts = 32;
		double[] adCos = new double[32];
		double[] adSin = new double[32];
		for ( int i = 0; i < numPts; i++ )
		{
			adCos[i] = Math.cos( Math.PI * 2.0 * i/numPts );
			adSin[i] = Math.sin( Math.PI * 2.0 * i/numPts);
		}
		float diameterB = diameterA/2f;// + (1-scale) * diameterA/4f;
		for ( int i = 0; i < numPts; i++ )
		{
			Vector3f pos1 = Vector3f.scale((float) (diameterA * adCos[i]), right);
			Vector3f pos2 = Vector3f.scale((float) (diameterB * adSin[i]), up);
			Vector3f pos = Vector3f.add(pos1,pos2);
			pos.add(center);
			ellipse.addElement( pos );
		}
		float[] extents = new float[]{diameterA, diameterB, 1 };
		Vector3f[] axes = new Vector3f[]{right, up, Vector3f.cross(right,up) };		
		return new Ellipsoid3f( center, axes, extents );
	}
        
    
    private void makeEllipse( Vector3f right, Vector3f up, Vector3f center, float diameter, VOIContour ellipse  )
	{
		int numPts = 12;
		for ( int i = 0; i < numPts; i++ )
		{
			double c = Math.cos( Math.PI * 2.0 * i/numPts );
			double s = Math.sin( Math.PI * 2.0 * i/numPts);
			Vector3f pos1 = Vector3f.scale((float) (diameter * c), right);
			Vector3f pos2 = Vector3f.scale((float) (diameter * s), up);
			Vector3f pos = Vector3f.add(pos1,pos2);
			pos.add(center);
			ellipse.addElement( pos );
		}
	}
    
    
    
    
    

    
    

    private void makeEllipse2( Vector3f right, Vector3f up, Vector3f center, float diameterA, float scale, VOIContour ellipse  )
	{
		int numPts = 32;
		float diameterB = diameterA/2f;// + (1-scale) * diameterA/4f;
		for ( int i = 0; i < numPts; i++ )
		{
			double c = Math.cos( Math.PI * 2.0 * i/numPts );
			double s = Math.sin( Math.PI * 2.0 * i/numPts);
			Vector3f pos1 = Vector3f.scale((float) (diameterA * c), right);
			Vector3f pos2 = Vector3f.scale((float) (diameterB * s), up);
			Vector3f pos = Vector3f.add(pos1,pos2);
			pos.add(center);
			ellipse.addElement( pos );
		}
	}
    
    private void saveAllVOIsTo(final String voiDir, ModelImage image) {
        try {
            ViewVOIVector VOIs = image.getVOIs();

            final File voiFileDir = new File(voiDir);

            if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
            	String[] list = voiFileDir.list();
            	for ( int i = 0; i < list.length; i++ )
            	{
            		File lrFile = new File( voiDir + list[i] );
            		lrFile.delete();
            	}
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

    
    private void saveLatticeStatistics( ModelImage image, float length, VOIContour left, VOIContour right, 
    		float[] leftPairs, float[] rightPairs, String postFix )
    {
    	String imageName = image.getImageName();
    	if ( imageName.contains("_clone") )
    	{
    		imageName = imageName.replaceAll("_clone", "" );
    	}
		String voiDir = image.getImageDirectory() + JDialogBase.makeImageName( imageName, "") + File.separator;
        File voiFileDir = new File(voiDir);
        if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
        } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
        } else { // voiFileDir does not exist
            voiFileDir.mkdir();
        }
		voiDir = image.getImageDirectory() + JDialogBase.makeImageName( imageName, "") + File.separator +
    			"statistics" + File.separator;
        voiFileDir = new File(voiDir);
        if (voiFileDir.exists() && voiFileDir.isDirectory()) {
//        	String[] list = voiFileDir.list();
//        	for ( int i = 0; i < list.length; i++ )
//        	{
//        		File lrFile = new File( voiDir + list[i] );
//        		lrFile.delete();
//        	}
        } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {
        } else { // voiFileDir does not exist
            voiFileDir.mkdir();
        }

        File file = new File(voiDir + "LatticeInfo" + postFix + ".csv");
        if ( file.exists() )
        {
        	file.delete();
        	file = new File(voiDir + "LatticeInfo" + postFix + ".csv");
        }


        try {

        	FileWriter fw = new FileWriter(file);
        	BufferedWriter bw = new BufferedWriter(fw);
        	bw.write( "Total Length:," +  VOILatticeManagerInterface.VoxelSize * length + "\n" );
            bw.newLine();
        	bw.write( "pair" + "," + "diameter" + ","  + "left distance" + "," + "right distance" + "\n" );
        	for ( int i = 0; i < left.size(); i++ )
        	{
        		bw.write(i + "," + VOILatticeManagerInterface.VoxelSize * left.elementAt(i).distance(right.elementAt(i)) + "," + VOILatticeManagerInterface.VoxelSize * leftPairs[i] + "," + VOILatticeManagerInterface.VoxelSize * rightPairs[i] + "\n");
        	}
            bw.newLine();
        	bw.close();
        } catch (final Exception e) {
        	System.err.println("CAUGHT EXCEPTION WITHIN writeXML() of FileVOI");
        	e.printStackTrace();
        }
    }
    
    private void saveLatticeStatistics( ModelImage image, ModelImage model, ModelImage originToStraight, VOIContour left, VOIContour right, int[][] volumes, String postFix )
    {    	
    	String imageName = image.getImageName();
    	if ( imageName.contains("_clone") )
    	{
    		imageName = imageName.replaceAll("_clone", "" );
    	}
		String voiDir = image.getImageDirectory() + JDialogBase.makeImageName( imageName, "") + File.separator;
        File voiFileDir = new File(voiDir);
        if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
        } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
        } else { // voiFileDir does not exist
            voiFileDir.mkdir();
        }
		voiDir = image.getImageDirectory() + JDialogBase.makeImageName( imageName, "") + File.separator +
    			"statistics" + File.separator;
        voiFileDir = new File(voiDir);
        if (voiFileDir.exists() && voiFileDir.isDirectory()) {
        } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {
        } else { // voiFileDir does not exist
            voiFileDir.mkdir();
        }

        File file = new File(voiDir + "LatticePositions" + postFix + ".csv");
        if ( file.exists() )
        {
        	file.delete();
        	file = new File(voiDir + "LatticePositions" + postFix + ".csv");
        }


        try {
        	float cubicVolume = VOILatticeManagerInterface.VoxelSize * VOILatticeManagerInterface.VoxelSize * VOILatticeManagerInterface.VoxelSize;
        	Vector3f transformedOrigin = new Vector3f();
        	if ( (model != null) && (originToStraight != null) && (wormOrigin != null) )
        	{
        		transformedOrigin = originToStraight( model, originToStraight, wormOrigin, "wormOrigin");
        	}

        	FileWriter fw = new FileWriter(file);
        	BufferedWriter bw = new BufferedWriter(fw);
        	bw.write( "name" + "," + "x_voxels" + "," + "y_voxels" + "," + "z_voxels" + "," + "x_um" + "," + "y_um" + "," + "z_um" + "," + "volume_voxel" + "," + "volume_um" + "\n" );
        	for ( int i = 0; i < left.size(); i++ )
        	{
            	Vector3f position = left.elementAt(i);
//            	if ( (model != null) && (originToStraight != null) )
//            	{
//                	position = originToStraight( model, originToStraight, position, "left"+i);
//            	}
        		bw.write("L"+i + "," + (position.X - transformedOrigin.X) + "," + 
        				(position.Y - transformedOrigin.Y) + "," + 
        				(position.Z - transformedOrigin.Z) + "," + 
        				
        				VOILatticeManagerInterface.VoxelSize * (position.X - transformedOrigin.X) + "," + 
        				VOILatticeManagerInterface.VoxelSize * (position.Y - transformedOrigin.Y) + "," + 
        				VOILatticeManagerInterface.VoxelSize * (position.Z - transformedOrigin.Z) + "," +
        				volumes[i][0] + "," +
        				cubicVolume * volumes[i][0] +
        				"\n");
        		

            	position = right.elementAt(i);
//            	if ( originToStraight != null )
//            	{
//                	position = originToStraight( model, originToStraight, position, "right"+i);
//            	}
        		bw.write("R"+i + "," + (position.X - transformedOrigin.X) + "," + 
        				(position.Y - transformedOrigin.Y) + "," + 
        				(position.Z - transformedOrigin.Z) + "," + 
        				
        				VOILatticeManagerInterface.VoxelSize * (position.X - transformedOrigin.X) + "," + 
        				VOILatticeManagerInterface.VoxelSize * (position.Y - transformedOrigin.Y) + "," + 
        				VOILatticeManagerInterface.VoxelSize * (position.Z - transformedOrigin.Z) + "," +
        				volumes[i][1] + "," +
        				cubicVolume * volumes[i][1] +
        				"\n");
        	}
            bw.newLine();
        	bw.close();
        } catch (final Exception e) {
        	System.err.println("CAUGHT EXCEPTION WITHIN writeXML() of FileVOI");
        	e.printStackTrace();
        }
    }
    
    private VOI saveAnnotationStatistics( ModelImage image, ModelImage model, ModelImage originToStraight, int[] outputDim, String postFix )
    {
    	if ( annotationVOIs == null )
    	{
    		return null;
    	}
    	if ( annotationVOIs.getCurves().size() == 0 )
    	{
    		return null;
    	}
    	
    	
    	String imageName = image.getImageName();
    	if ( imageName.contains("_clone") )
    	{
    		imageName = imageName.replaceAll("_clone", "" );
    	}
		String voiDir = image.getImageDirectory() + JDialogBase.makeImageName( imageName, "") + File.separator;
        File voiFileDir = new File(voiDir);
        if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
        } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
        } else { // voiFileDir does not exist
            voiFileDir.mkdir();
        }
		voiDir = image.getImageDirectory() + JDialogBase.makeImageName( imageName, "") + File.separator +
    			"statistics" + File.separator;
        voiFileDir = new File(voiDir);
        if (voiFileDir.exists() && voiFileDir.isDirectory()) {
        } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {
        } else { // voiFileDir does not exist
            voiFileDir.mkdir();
        }

        File file = new File(voiDir + "AnnotationInfo" + postFix + ".csv");
        if ( file.exists() )
        {
        	file.delete();
        	file = new File(voiDir + "AnnotationInfo" + postFix + ".csv");
        }


        VOI transformedAnnotations = null;
        try {
        	Vector3f transformedOrigin = new Vector3f();
        	if ( (model != null) && (originToStraight != null) && (wormOrigin != null) )
        	{
        		transformedOrigin = originToStraight(model, originToStraight, wormOrigin, "wormOrigin");
        	}
        	if ( originToStraight != null )
        	{
        		transformedAnnotations = new VOI( annotationVOIs );
        	}

        	FileWriter fw = new FileWriter(file);
        	BufferedWriter bw = new BufferedWriter(fw);
        	bw.write( "name" + "," + "x_voxels" + "," + "y_voxels" + "," + "z_voxels" + "," + "x_um" + "," + "y_um" + "," + "z_um" + "\n" );
        	for ( int i = 0; i < annotationVOIs.getCurves().size(); i++ )
        	{
            	VOIText text = (VOIText) annotationVOIs.getCurves().elementAt(i);
            	Vector3f position = text.elementAt(0);
            	if ( (model != null) && (originToStraight != null) )
            	{
                	position = originToStraight( model, originToStraight, position, text.getText());

		    		transformedAnnotations.getCurves().elementAt(i).elementAt(0).copy( position );
		    		transformedAnnotations.getCurves().elementAt(i).elementAt(1).set( position.X + 5 , position.Y, position.Z );
            	}
        		bw.write(text.getText() + "," + (position.X - transformedOrigin.X) + "," + 
        				(position.Y - transformedOrigin.Y) + "," + 
        				(position.Z - transformedOrigin.Z) + "," + 
        				
        				VOILatticeManagerInterface.VoxelSize * (position.X - transformedOrigin.X) + "," + 
        				VOILatticeManagerInterface.VoxelSize * (position.Y - transformedOrigin.Y) + "," + 
        				VOILatticeManagerInterface.VoxelSize * (position.Z - transformedOrigin.Z) +
        				"\n");
        	}
            bw.newLine();
        	bw.close();
        } catch (final Exception e) {
        	System.err.println("CAUGHT EXCEPTION WITHIN writeXML() of FileVOI");
        	e.printStackTrace();
        }
        
        return transformedAnnotations;
    }
    
    private Vector3f originToStraight( ModelImage model, ModelImage originToStraight, Vector3f pt, String text )
    {
    	int x = Math.round(pt.X);
    	int y = Math.round(pt.Y);
    	int z = Math.round(pt.Z);

		float outputA = originToStraight.getFloatC( x, y, z, 0);
		float outputX = originToStraight.getFloatC( x, y, z, 1);
		float outputY = originToStraight.getFloatC( x, y, z, 2);
		float outputZ = originToStraight.getFloatC( x, y, z, 3);
    	
		if ( outputA == 0 )
		{
			float m = model.getFloat(x,y,z);
			if ( m != 0 )
			{		    	
		    	int dimX = model.getExtents().length > 0 ? model.getExtents()[0] : 1;
		    	int dimY = model.getExtents().length > 1 ? model.getExtents()[1] : 1;
		    	int dimZ = model.getExtents().length > 2 ? model.getExtents()[2] : 1;
				
				int count = 0;
				Vector3f pts = new Vector3f();
				for ( int z1 = Math.max(0, z-2); z1 < Math.min(dimZ, z+2); z1++ )
				{
					for ( int y1 = Math.max(0, y-2); y1 < Math.min(dimY, y+2); y1++ )
					{
						for ( int x1 = Math.max(0, x-2); x1 < Math.min(dimX, x+2); x1++ )
    					{
		    				float a1 = originToStraight.getFloatC(x1, y1, z1, 0);
							float m1 = model.getFloat(x1,y1,z1);
							if ( (a1 != 0) && (m1 == m) )
							{
			    				float x2 = originToStraight.getFloatC(x1, y1, z1, 1);
			    				float y2 = originToStraight.getFloatC(x1, y1, z1, 2);
			    				float z2 = originToStraight.getFloatC(x1, y1, z1, 3);
			    				pts.add( x2,y2,z2 );
			    				count++;
							}
    					}
					}
				}
				if ( count != 0 )
				{
//					System.err.println( imageA.getImageName() + " originToStraight " + text + " " + pt + " OK ");
					pts.scale(1f/(float)count);
					return pts;
				}
			}
			else
			{
				System.err.println( imageA.getImageName() + " originToStraight " + text + " " + pt );
			}
		}
		
		return new Vector3f( outputX, outputY, outputZ );
    }
    
    private void saveTransformImage( String imageName, ModelImage image, boolean saveAsTif  ) 
	{
		String voiDir = image.getImageDirectory() + JDialogBase.makeImageName( imageName, "") + File.separator;
        File voiFileDir = new File(voiDir);
        if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
        } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
        } else { // voiFileDir does not exist
            voiFileDir.mkdir();
        }
		voiDir = image.getImageDirectory() + JDialogBase.makeImageName( imageName, "") + File.separator +
    			"output_images" + File.separator;
        voiFileDir = new File(voiDir);
        if (voiFileDir.exists() && voiFileDir.isDirectory()) {
        } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {
        } else { // voiFileDir does not exist
            voiFileDir.mkdir();
        }
        
        File file = new File(voiDir + imageName);
        if ( file.exists() )
        {
        	file.delete();
        }
//        System.err.println( voiDir );
//        System.err.println( image.getImageName() + ".xml" );
        ModelImage.saveImage( image, image.getImageName() + ".xml", voiDir, false );
        if ( saveAsTif )
        {
            ModelImage.saveImage( image, image.getImageName() + ".tif", voiDir, false );        	
        }
    }
    private NaturalSpline3 smoothCurve( VOIContour curve, float[] time )
    {
    	float totalDistance = 0;
    	for ( int i = 0; i < curve.size()-1; i++ )
    	{
    		totalDistance += curve.elementAt(i).distance(curve.elementAt(i+1));
    	}
    	
    	Vector3f[] akPoints = new Vector3f[curve.size()];
    	float distance = 0;
    	for ( int i = 0; i < curve.size(); i++ )
    	{
    		if ( i > 0 )
    		{
    			distance += curve.elementAt(i).distance( curve.elementAt(i-1) );
    			time[i] = distance / totalDistance;
    			akPoints[i] = new Vector3f(curve.elementAt(i));
    		}
    		else
    		{    			
    			time[i] = 0;
    			akPoints[i] = new Vector3f(curve.elementAt(i));
    		}
    	}
    	
    	return new NaturalSpline3( NaturalSpline3.BoundaryType.BT_FREE, curve.size()-1, time, akPoints );
    }
    
//    private void saveMesh( ModelImage image, TriMesh mesh, final boolean flip ) 
//	{
//    	int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
//    	int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
//    	int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;   
//    	
//        TransMatrix dicomMatrix = null;
//        TransMatrix inverseDicomMatrix = null;
//        // double[][] inverseDicomArray = null;
//        float[] coord;
//        float[] tCoord;
//
//		int iVQuantity = mesh.VBuffer.GetVertexQuantity();
//
//    	float[] res = image.getResolutions(0);
//        float[] startLocation = image.getFileInfo()[0].getOrigin();
//        int[] direction = MipavCoordinateSystems.getModelDirections(image);
//        
//        Vector3f[] transformedPositions = new Vector3f[iVQuantity];
//        if ( image.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL) )
//        {
//
//            // Get the DICOM transform that describes the transformation from
//            // axial to this image orientation
//            dicomMatrix = image.getMatrix();
//            inverseDicomMatrix = new TransMatrix(image.getMatrix());
//            inverseDicomMatrix.Inverse();
//            // inverseDicomArray = inverseDicomMatrix.getMatrix();
//            // inverseDicomMatrix = null;
//            coord = new float[3];
//            tCoord = new float[3];
//
//            for ( int i = 0; i < iVQuantity; i++)
//            {
//            	Vector3f pos = mesh.VBuffer.GetPosition3(i);
//            	
//                // Change the voxel coordinate into millimeter space
//                coord[0] = pos.X * res[0];
//                coord[1] = pos.Y * res[1];
//                coord[2] = pos.Z * res[2];
//
//                // Convert the point to axial millimeter DICOM space
//                dicomMatrix.transform(coord, tCoord);
//
//                // Add in the DICOM origin
//                pos.X = startLocation[0] + tCoord[0];
//                pos.Y = startLocation[1] + tCoord[1];
//                pos.Z = startLocation[2] + tCoord[2];
//                transformedPositions[i] = pos;
//            }
//        }
//        else
//        {
//            for ( int i = 0; i < iVQuantity; i++ )
//            {
//            	Vector3f pos = mesh.VBuffer.GetPosition3(i);
//            	pos.X = (pos.X * res[0] * direction[0]) + startLocation[0];
//            	pos.Y = (pos.Y * res[1] * direction[1]) + startLocation[1];
//            	pos.Z = (pos.Z * res[2] * direction[2]) + startLocation[2];
//            	transformedPositions[i] = pos;
//            }
//        }
//
//
//        float[] box = new float[3];
//        box[0] = (dimX - 1) * res[0];
//        box[1] = (dimY - 1) * res[1];
//        box[2] = (dimZ - 1) * res[2];
//        
//        
//
//    	String imageName = image.getImageName();
//    	if ( imageName.contains("_clone") )
//    	{
//    		imageName = imageName.replaceAll("_clone", "" );
//    	}
//		String voiDir = image.getImageDirectory() + JDialogBase.makeImageName( imageName, "") + File.separator;
//        File voiFileDir = new File(voiDir);
//        if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
//        } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
//        } else { // voiFileDir does not exist
//            voiFileDir.mkdir();
//        }
//		voiDir = image.getImageDirectory() + JDialogBase.makeImageName( imageName, "") + File.separator +
//    			"worm_model" + File.separator;
//        voiFileDir = new File(voiDir);
//        if (voiFileDir.exists() && voiFileDir.isDirectory()) {
////        	String[] list = voiFileDir.list();
////        	for ( int i = 0; i < list.length; i++ )
////        	{
//////        		System.err.println( list[i] );
////        		File meshFile = new File( voiDir + list[i] );
////        		meshFile.delete();
////        	}
//        } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {
//        } else { // voiFileDir does not exist
//            voiFileDir.mkdir();
//        }
//        
//        String kName = voiDir + "worm_surface.xml";
//        
//        TriMesh kMesh = new TriMesh( new VertexBuffer(transformedPositions), new IndexBuffer(mesh.IBuffer), false);
//        try {
//			FileSurface_WM.save(kName, kMesh, 0, kMesh.VBuffer, flip, direction, startLocation, box, inverseDicomMatrix);
//		} catch (IOException e) {}
//    }    
    
    private NaturalSpline3 smoothCurve2( VOIContour curve, float[] time )
    {
    	Vector3f[] akPoints = new Vector3f[curve.size()];
    	for ( int i = 0; i < curve.size(); i++ )
    	{
    		akPoints[i] = new Vector3f(curve.elementAt(i));
    	}
    	
    	return new NaturalSpline3( NaturalSpline3.BoundaryType.BT_FREE, curve.size()-1, time, akPoints );
    }
    
    
//    private boolean testIntersections( TriMesh mesh  )
//	{
//		int numTriangles = mesh.GetTriangleQuantity();
//		int[] indices = mesh.IBuffer.GetData();
//
//	    for (int i = 0; i < numTriangles; ++i)
//	    {
//	        int v0 = indices[i*3 + 0];
//	        int v1 = indices[i*3 + 1];
//	        int v2 = indices[i*3 + 2];
//	        Vector3f p0 = mesh.VBuffer.GetPosition3( v0 );
//	        Vector3f p1 = mesh.VBuffer.GetPosition3( v1 );
//	        Vector3f p2 = mesh.VBuffer.GetPosition3( v2 );
//	        Triangle3f t0 = new Triangle3f(p0, p1, p2);
//	        
//		    for (int j = i+1; j < numTriangles; ++j)
//		    {
//		        int v10 = indices[j*3 + 0];
//		        int v11 = indices[j*3 + 1];
//		        int v12 = indices[j*3 + 2];
//		        if ( v0 == v10 || v0 == v11 || v0 == v12 ||
//			         v1 == v10 || v1 == v11 || v1 == v12 ||
//			         v2 == v10 || v2 == v11 || v2 == v12 )
//		        {
//		        	continue;
//		        }
//		        
//		        
//		        Vector3f p10 = mesh.VBuffer.GetPosition3( v10 );
//		        Vector3f p11 = mesh.VBuffer.GetPosition3( v11 );
//		        Vector3f p12 = mesh.VBuffer.GetPosition3( v12 );
//		        
//		        if ( p0.isEqual(p10) || p0.isEqual(p11) || p0.isEqual(p12) ||
//			         p1.isEqual(p10) || p1.isEqual(p11) || p1.isEqual(p12) ||
//			         p2.isEqual(p10) || p2.isEqual(p11) || p2.isEqual(p12) )
//		        {
//		        	continue;
//		        }
//		        
//		        
//		        Triangle3f t1 = new Triangle3f(p10, p11, p12);
//		        
//		        IntrTriangle3Triangle3f intersector = new IntrTriangle3Triangle3f( t0, t1 );
//		        if ( intersector.Test() )
//		        {			        
//			        if ( intersector.Find() )
//			        {
//			        	return true;
//			        }
//		        }
//		    }
//	    }
//
//	    return false;
//	}
    
    
    private void straighten( ModelImage image, int[] resultExtents, String baseName, ModelImage model, boolean saveStats, boolean displayResult, boolean saveAsTif )
    {
    	String imageName = image.getImageName();
    	if ( imageName.contains("_clone") )
    	{
    		imageName = imageName.replaceAll("_clone", "" );
    	}

		int colorFactor = image.isColorImage() ? 4 : 1;
		float[] values = new float[resultExtents[0] * resultExtents[1] * colorFactor]; 

		float[] dataOrigin = null;
		float[] sampleDistance = null;
		float[] sampleDistanceP = null;
		

		ModelImage resultImage = new ModelImage(image.getType(), resultExtents, imageName + "_straight.xml");
		JDialogBase.updateFileInfo( image, resultImage );
		resultImage.setResolutions( new float[]{1,1,1});
		
		ModelImage straightToOrigin = null;
		ModelImage originToStraight = null;
		ModelImage overlap2 = null;
		
		if ( saveStats )
		{
			dataOrigin = new float[resultExtents[0] * resultExtents[1] * 4]; 
			straightToOrigin =new ModelImage( ModelStorageBase.ARGB_FLOAT, resultExtents, imageName + "_toOriginal.xml");
			JDialogBase.updateFileInfo( image, straightToOrigin );
			straightToOrigin.setResolutions( new float[]{1,1,1});
			for ( int i = 0; i < straightToOrigin.getDataSize(); i++ )
			{
				straightToOrigin.set(i, 0);
			}
			
	    	originToStraight = new ModelImage( ModelStorageBase.ARGB_FLOAT, image.getExtents(),  imageName + "_toStraight.xml");
			JDialogBase.updateFileInfo( image, originToStraight );
			for ( int i = 0; i < originToStraight.getDataSize(); i++ )
			{
				originToStraight.set(i, 0);
			}

			overlap2 = new ModelImage( ModelStorageBase.FLOAT, resultExtents, imageName + "_sampleDensity.xml");
			JDialogBase.updateFileInfo( image, overlap2 );
			overlap2.setResolutions( new float[]{1,1,1});

			sampleDistance = new float[resultExtents[0] * resultExtents[1]]; 
			sampleDistanceP = new float[resultExtents[0] * resultExtents[1] * 4]; 
			int length = resultExtents[0] * resultExtents[1];
			for ( int j = 0; j < length; j++ )
			{       		
				sampleDistance[j] = 0;
				for ( int c = 0; c < 4; c++ )
				{
//					sampleDistance[j * 4 + c] = 0;
					sampleDistanceP[j * 4 + c] = 0;
				}
			}
		}

		for( int i = 0; i < samplingPlanes.getCurves().size(); i++ )
		{
			VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
	        Vector3f[] corners = new Vector3f[4];
	        for ( int j = 0; j < 4; j++ )
	        {
	        	corners[j] = kBox.elementAt(j);
	        }
			float planeDist = -Float.MAX_VALUE;
	        if ( i < (samplingPlanes.getCurves().size() - 1) )
	        {
				kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i+1);
		        for ( int j = 0; j < 4; j++ )
		        {
		        	float distance = corners[j].distance(kBox.elementAt(j));
		        	if ( distance > planeDist )
		        	{
		        		planeDist = distance;
		        	}
//		        	System.err.println( distance + "  " + centerPositions.elementAt(i).distance( centerPositions.elementAt(i+1) ) );
		        }	        	
//		        System.err.println("");
	        }
			try {
				for ( int j = 0; j < values.length/colorFactor; j++ )
				{
                	if ( colorFactor == 4 ) {
                		values[ (j * 4) + 0] = (float)image.getMinA();
                		values[ (j * 4) + 1] = (float)image.getMinR();
                		values[ (j * 4) + 2] = (float)image.getMinG();
                		values[ (j * 4) + 3] = (float)image.getMinB();
                	}
                	/* not color: */
                	else {
                		values[j] = (float)image.getMin();
                	}                		
					if ( dataOrigin != null )
					{
						for ( int c = 0; c < 4; c++ )
						{
							dataOrigin[j * 4 + c] = 0;
						}
					}
				}

				int planeCount = 0;
		        if ( i < (samplingPlanes.getCurves().size() - 1) )
		        {
		        	planeDist *= 3;
		        	kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i+1);
			        Vector3f[] steps = new Vector3f[4];
			        Vector3f[] cornersSub = new Vector3f[4];
			        for ( int j = 0; j < 4; j++ )
			        {
			        	steps[j] = Vector3f.sub( kBox.elementAt(j), corners[j] ); steps[j].scale(1f/planeDist);
				        cornersSub[j] = new Vector3f(corners[j]);
			        }
		        	for ( int j = 0; j < planeDist; j++ )
		        	{
		        		writeDiagonal( image, model, originToStraight, 0, i, resultExtents, cornersSub, values, dataOrigin, sampleDistance, sampleDistanceP);
		        		planeCount++;
				        for ( int k = 0; k < 4; k++ )
				        {
					        cornersSub[k].add(steps[k]);
				        }
		        	}
		        }
		        else
		        {
		        	planeDist = 15;
		        	kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i-1);
			        Vector3f[] steps = new Vector3f[4];
			        Vector3f[] cornersSub = new Vector3f[4];
			        for ( int j = 0; j < 4; j++ )
			        {
			        	steps[j] = Vector3f.sub( corners[j], kBox.elementAt(j) ); steps[j].scale(1f/planeDist);
//				        cornersSub[j] = Vector3f.add( corners[j], kBox.elementAt(j) ); cornersSub[j].scale(0.5f);
				        cornersSub[j] = new Vector3f(corners[j]);
			        }
		        	for ( int j = 0; j < planeDist; j++ )
		        	{
		        		writeDiagonal( image, model, originToStraight, 0, i, resultExtents, cornersSub, values, dataOrigin, sampleDistance, sampleDistanceP);
		        		planeCount++;
				        for ( int k = 0; k < 4; k++ )
				        {
					        cornersSub[k].add(steps[k]);
				        }
		        	}
//					writeDiagonal( image, model, originToStraight, 0, i, resultExtents, corners, values, dataOrigin);		        	
		        }
				for ( int j = 0; j < values.length/colorFactor; j++ )
				{
                	if ( colorFactor == 4 ) {
                		values[ (j * 4) + 1] /= (float)planeCount;
                		values[ (j * 4) + 2] /= (float)planeCount;
                		values[ (j * 4) + 3] /= (float)planeCount;
                	}
                	/* not color: */
                	else {
                		values[j] /= (float)planeCount;
                	}                		
					if ( dataOrigin != null )
					{
						for ( int c = 1; c < 4; c++ )
						{
							dataOrigin[j * 4 + c] /= (float)planeCount;
						}
					}
				}
		        
		        
				
				resultImage.importData(i*values.length, values, false);
				if ( straightToOrigin != null )
				{
					straightToOrigin.importData(i*dataOrigin.length, dataOrigin, false);
				}
				if ( overlap2 != null )
				{
					overlap2.importData(i*sampleDistance.length, sampleDistance, false);
				}
				
			} catch(IOException e) {
				e.printStackTrace();
			}
		}
		
//		System.err.println( "straighten " +	planeCount );
//		ModelImage resultImage2 = new ModelImage(image.getType(), new int[]{resultExtents[0], resultExtents[1], planeCount}, imageName + "_straight.xml");
//		JDialogBase.updateFileInfo( image, resultImage2 );
//		resultImage2.setResolutions( new float[]{1,1,1});;
//		planeCount = 0;
//		for( int i = 0; i < samplingPlanes.getCurves().size(); i++ )
//		{
//			VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
//	        Vector3f[] corners = new Vector3f[4];
//	        for ( int j = 0; j < 4; j++ )
//	        {
//	        	corners[j] = kBox.elementAt(j);
//	        }
//			float planeDist = -Float.MAX_VALUE;
//	        if ( i < (samplingPlanes.getCurves().size() - 1) )
//	        {
//				kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i+1);
//		        for ( int j = 0; j < 4; j++ )
//		        {
//		        	float distance = corners[j].distance(kBox.elementAt(j));
//		        	if ( distance > planeDist )
//		        	{
//		        		planeDist = distance;
//		        	}
////		        	System.err.println( distance + "  " + centerPositions.elementAt(i).distance( centerPositions.elementAt(i+1) ) );
//		        }	        	
////		        System.err.println("");
//	        }
//			try {
//
//		        if ( i < (samplingPlanes.getCurves().size() - 1) )
//		        {
//		        	planeDist *= 3;
//		        	kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i+1);
//			        Vector3f[] steps = new Vector3f[4];
//			        Vector3f[] cornersSub = new Vector3f[4];
//			        for ( int j = 0; j < 4; j++ )
//			        {
//			        	steps[j] = Vector3f.sub( kBox.elementAt(j), corners[j] ); steps[j].scale(1f/planeDist);
//				        cornersSub[j] = new Vector3f(corners[j]);
//			        }
//		        	for ( int j = 0; j < planeDist; j++ )
//		        	{
//		        		writeDiagonal( image, model, originToStraight, 0, i, resultExtents, cornersSub, values, dataOrigin);
//						resultImage2.importData(planeCount++*values.length, values, false);
//				        for ( int k = 0; k < 4; k++ )
//				        {
//					        cornersSub[k].add(steps[k]);
//				        }
//				        
//				        
//
//						for ( int k = 0; k < values.length/colorFactor; k++ )
//						{
//		                	if ( colorFactor == 4 ) {
//		                		values[ (k * 4) + 0] = (float)image.getMinA();
//		                		values[ (k * 4) + 1] = (float)image.getMinR();
//		                		values[ (k * 4) + 2] = (float)image.getMinG();
//		                		values[ (k * 4) + 3] = (float)image.getMinB();
//		                	}
//		                	/* not color: */
//		                	else {
//		                		values[k] = (float)image.getMin();
//		                	}                		
//							if ( dataOrigin != null )
//							{
//								for ( int c = 0; c < 4; c++ )
//								{
//									dataOrigin[k * 4 + c] = 0;
//								}
//							}
//						}
//				        
//				        
//		        	}
//		        }
//		        else
//		        {
//		        	planeDist = 15;
//		        	kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i-1);
//			        Vector3f[] steps = new Vector3f[4];
//			        Vector3f[] cornersSub = new Vector3f[4];
//			        for ( int j = 0; j < 4; j++ )
//			        {
//			        	steps[j] = Vector3f.sub( corners[j], kBox.elementAt(j) ); steps[j].scale(1f/planeDist);
////				        cornersSub[j] = Vector3f.add( corners[j], kBox.elementAt(j) ); cornersSub[j].scale(0.5f);
//				        cornersSub[j] = new Vector3f(corners[j]);
//			        }
//		        	for ( int j = 0; j < planeDist; j++ )
//		        	{
//		        		writeDiagonal( image, model, originToStraight, 0, i, resultExtents, cornersSub, values, dataOrigin);
//						resultImage2.importData(planeCount++*values.length, values, false);
//				        for ( int k = 0; k < 4; k++ )
//				        {
//					        cornersSub[k].add(steps[k]);
//				        }
//				        
//
//						for ( int k = 0; k < values.length/colorFactor; k++ )
//						{
//		                	if ( colorFactor == 4 ) {
//		                		values[ (k * 4) + 0] = (float)image.getMinA();
//		                		values[ (k * 4) + 1] = (float)image.getMinR();
//		                		values[ (k * 4) + 2] = (float)image.getMinG();
//		                		values[ (k * 4) + 3] = (float)image.getMinB();
//		                	}
//		                	/* not color: */
//		                	else {
//		                		values[k] = (float)image.getMin();
//		                	}                		
//							if ( dataOrigin != null )
//							{
//								for ( int c = 0; c < 4; c++ )
//								{
//									dataOrigin[k * 4 + c] = 0;
//								}
//							}
//						}
//		        	}
////					writeDiagonal( image, model, originToStraight, 0, i, resultExtents, corners, values, dataOrigin);		        	
//		        }
//				
//				
//			} catch(IOException e) {
//				e.printStackTrace();
//			}
//		}
//		if ( displayResult )
//		{
//			resultImage2.calcMinMax();
//			new ViewJFrameImage(resultImage2);
//		}
		
		
		
		
		VOI transformedAnnotations = null;
		if ( saveStats && (straightToOrigin != null) )
		{
			testOriginToStraight( model, originToStraight );
				
//			float outputDist = -Float.MAX_VALUE;
//			
//			ModelImage overlap2 = new ModelImage( ModelStorageBase.ARGB_FLOAT, resultExtents, imageName + "_sampleDensity.xml");
//			JDialogBase.updateFileInfo( image, overlap2 );
//			overlap2.setResolutions( new float[]{1,1,1});
//			for ( int z = 0; z < resultExtents[2]; z++ )
//			{
//				for ( int x = 0; x < resultExtents[0]; x++ )
//				{
//					for ( int y = 0; y < resultExtents[1]; y++ )
//					{
//						if ( z + 1 < resultExtents[2] )
//						{
//							Vector3f pos0 = new Vector3f( straightToOrigin.getFloatC(x, y, z, 1),   straightToOrigin.getFloatC(x, y, z, 2),   straightToOrigin.getFloatC(x, y, z, 3) );
//							Vector3f pos1 = new Vector3f( straightToOrigin.getFloatC(x, y, z+1, 1), straightToOrigin.getFloatC(x, y, z+1, 2), straightToOrigin.getFloatC(x, y, z+1, 3) );
//							if ( !pos0.isEqual(Vector3f.ZERO) && !pos1.isEqual(Vector3f.ZERO))
//							{
//								float diff = pos1.distance(pos0);
////								if ( diff > 1.5 )
//								if ( diff > 1.0 )
//								{
//									overlap2.setC(x, y, z, 0, 1);
//									overlap2.setC(x, y, z, 1, 0);
//									overlap2.setC(x, y, z, 2, 0);
//									overlap2.setC(x, y, z, 3, diff - 1.0);
//								}
//								else if ( diff < 1.0)
////									else if ( diff < .5)
//								{
//									overlap2.setC(x, y, z, 0, 1);
//									overlap2.setC(x, y, z, 1, 1.0 - diff);
//									overlap2.setC(x, y, z, 2, 0);
//									overlap2.setC(x, y, z, 3, 0);
//								}
//								else
//								{
//									overlap2.setC(x, y, z, 0, 0);
//									overlap2.setC(x, y, z, 1, 0);
//									overlap2.setC(x, y, z, 2, 0);
//									overlap2.setC(x, y, z, 3, 0);									
//								}
//								if ( diff > outputDist )
//								{
//									outputDist = diff;
//								}
//							}
//							else
//							{
//								overlap2.setC(x, y, z, 0, 0);
//								overlap2.setC(x, y, z, 1, 0);
//								overlap2.setC(x, y, z, 2, 0);
//								overlap2.setC(x, y, z, 3, 0);
//							}
//						}
//						else
//						{
//							overlap2.setC(x, y, z, 0, 0);
//							overlap2.setC(x, y, z, 1, 0);
//							overlap2.setC(x, y, z, 2, 0);
//							overlap2.setC(x, y, z, 3, 0);
//						}
//					}
//				}
//			}
////			System.err.println( "Output plane distance " + outputDist );
//			
//			overlap2.calcMinMax();
//			float maxR = (float) overlap2.getMaxR();
//			float maxB = (float) overlap2.getMaxB();
////			System.err.println( maxR + " " + maxB );
//			for ( int z = 0; z < resultExtents[2]; z++ )
//			{
//				for ( int x = 0; x < resultExtents[0]; x++ )
//				{
//					for ( int y = 0; y < resultExtents[1]; y++ )
//					{
//						float r = overlap2.getFloatC(x, y, z, 1);
//						overlap2.setC(x, y, z, 1, r/maxR);
//						float b = overlap2.getFloatC(x, y, z, 3);
//						overlap2.setC(x, y, z, 3, b/maxB);
//					}
//				}
//			}
			
			saveTransformImage(baseName, overlap2, true);
			
			

			saveTransformImage(baseName, straightToOrigin, false);
			saveTransformImage(baseName, originToStraight, false);
			transformedAnnotations = saveAnnotationStatistics( imageA, model, originToStraight, resultExtents, "_after");
			
//			testTransform( overlap2, straightToOrigin, image.getExtents(), image.getResolutions(0) );
			//		testTransform( image, originToStraight, resultImage.getExtents() );
			straightToOrigin.disposeLocal();
			straightToOrigin = null;
			
			overlap2.disposeLocal();
			overlap2 = null;


			short id = (short) image.getVOIs().getUniqueID();
			VOI lattice = new VOI(id, "lattice", VOI.POLYLINE, (float)Math.random() );
			VOIContour leftSide = new VOIContour( false );
			VOIContour rightSide = new VOIContour( false );
			lattice.getCurves().add(leftSide);		
			lattice.getCurves().add(rightSide);
			for ( int i = 0; i < left.size(); i++ )
			{			
				
				// USE backup makerers
				Vector3f leftPt = originToStraight( model, originToStraight, leftBackup.elementAt(i), "left"+i );

				// USE backup makerers
				Vector3f rightPt = originToStraight( model, originToStraight, rightBackup.elementAt(i), "right"+i );

				if ( leftPt.isEqual(Vector3f.ZERO) || rightPt.isEqual(Vector3f.ZERO))
				{
					System.err.println( "    " + imageA.getImageName() + " " + i + " " + leftPt + "     " + rightPt );
				}
				else if ( (leftSide.size() > 0) && ((leftPt.Z <= leftSide.lastElement().Z)  || (rightPt.Z <= rightSide.lastElement().Z)) )
				{
					System.err.println( "    " + imageA.getImageName() + " " + i + " " + leftPt + "     " + rightPt );
				}
				else
				{
					leftSide.add(leftPt);
					rightSide.add(rightPt);
				}
			}
			float[] leftDistances = new float[leftSide.size()];
			float[] rightDistances = new float[leftSide.size()];
			for ( int i = 0; i < leftSide.size(); i++ )
			{			
				leftDistances[i] = 0;
				rightDistances[i] = 0;
				if ( i > 1 )
				{
					leftDistances[i] = leftSide.elementAt(i).distance(leftSide.elementAt(i-1) );
					rightDistances[i] = rightSide.elementAt(i).distance(rightSide.elementAt(i-1) );
				}				
			}

			resultImage.registerVOI(lattice);
			lattice.setColor( new Color( 0, 0, 255) );
			lattice.getCurves().elementAt(0).update( new ColorRGBA(0,0,1,1));
			lattice.getCurves().elementAt(1).update( new ColorRGBA(0,0,1,1));
			lattice.getCurves().elementAt(0).setClosed(false);
			lattice.getCurves().elementAt(1).setClosed(false);


			id = (short) image.getVOIs().getUniqueID();
			//		VOI text = new VOI(id, "left_right", VOI.ANNOTATION, (float)Math.random() );
			//		resultImage.registerVOI( text );
			for ( int j = 0; j < leftSide.size(); j++ )
			{
				id = (short) image.getVOIs().getUniqueID();
				VOI marker = new VOI(id, "pair_" + j, VOI.POLYLINE, (float)Math.random() );
				VOIContour mainAxis = new VOIContour(false); 		    		    		
				mainAxis.add( leftSide.elementAt(j) );
				mainAxis.add( rightSide.elementAt(j) );
				marker.getCurves().add(mainAxis);
				marker.setColor( new Color( 255, 255, 0) );
				mainAxis.update( new ColorRGBA(1,1,0,1));
				if ( j == 0 )
				{
					marker.setColor( new Color( 0, 255, 0) );
					mainAxis.update( new ColorRGBA(0,1,0,1));
				}
				resultImage.registerVOI( marker );

				//			VOIText labelL = new VOIText();
				//			labelL.add( leftSide.elementAt(j) );
				//			labelL.add( Vector3f.add(leftSide.elementAt(j), new Vector3f(5, 0, 0) ) );
				//			labelL.setText("L"+j);
				//			text.getCurves().add(labelL);
				//
				//			VOIText labelR = new VOIText();
				//			labelR.add( rightSide.elementAt(j) );
				//			labelR.add( Vector3f.add(rightSide.elementAt(j), new Vector3f(5, 0, 0) ) );
				//			labelR.setText("R"+j);
				//			text.getCurves().add(labelR);
			}
		
			String voiDir = resultImage.getImageDirectory() + JDialogBase.makeImageName( baseName, "") + File.separator +
	    			"straightened_lattice" + File.separator;
			saveAllVOIsTo( voiDir, resultImage );  

			VOIVector temp = resultImage.getVOIsCopy();
			resultImage.resetVOIs();
			if ( transformedAnnotations != null )
			{
				resultImage.registerVOI( transformedAnnotations );
				voiDir = resultImage.getImageDirectory() + JDialogBase.makeImageName( baseName, "") + File.separator +
						"straightened_annotations" + File.separator;
				saveAllVOIsTo( voiDir, resultImage );  
			}
			saveLatticeStatistics(image, resultExtents[2], leftSide, rightSide, leftDistances, rightDistances, "_after");
			
			resultImage.restoreVOIs(temp);
			if ( transformedAnnotations != null )
			{
				resultImage.registerVOI( transformedAnnotations );
			}
						
			int[] markerIDs = new int[leftSide.size()];
			int[][] markerVolumes = new int[leftSide.size()][2];
			ModelImage straightMarkers = segmentMarkers( resultImage, leftSide, rightSide, markerIDs, markerVolumes, false );
			saveLatticeStatistics( imageA, model, originToStraight, leftSide, rightSide, markerVolumes, "_after" );

			saveTransformImage(baseName, straightMarkers, true);
			if ( displayResult )
			{
				straightMarkers.calcMinMax();
				new ViewJFrameImage(straightMarkers);
			}
			else
			{
				straightMarkers.disposeLocal();
				straightMarkers = null;
			}
		}

		saveTransformImage(baseName, resultImage, saveAsTif);
		if ( displayResult )
		{
			resultImage.calcMinMax();
			new ViewJFrameImage(resultImage);
		}
		else
		{
			resultImage.disposeLocal();
			resultImage = null;
		}
		
		if (originToStraight != null)
		{
			originToStraight.disposeLocal();
			originToStraight = null;
		}
    }
        
    
    private void testTransform( ModelImage original, ModelImage transform, int[] outputExtents, float[] outputRes )
    {
    	ModelImage output = new ModelImage( original.getType(), outputExtents, original.getImageName() + "_test_transformed" );
    	output.setResolutions(outputRes);
		
    	int dimX = transform.getExtents().length > 0 ? transform.getExtents()[0] : 1;
    	int dimY = transform.getExtents().length > 1 ? transform.getExtents()[1] : 1;
    	int dimZ = transform.getExtents().length > 2 ? transform.getExtents()[2] : 1;
    	DataType bufferType = original.getDataType();
    	
    	for ( int z = 0; z < dimZ; z++ )
    	{
    		for ( int y = 0; y < dimY; y++ )
    		{
    			for ( int x = 0; x < dimX; x++ )
    			{
    				float a = transform.getFloatC(x, y, z, 0);
    				if ( a == 1 )
    				{
    					int inputIndex = z * dimX * dimY + y * dimX + x;
    					int outputX = Math.round(transform.getFloat( (inputIndex * 4) + 1));
    					int outputY = Math.round(transform.getFloat( (inputIndex * 4) + 2));
    					int outputZ = Math.round(transform.getFloat( (inputIndex * 4) + 3));
    					
    					int outputIndex = outputZ * outputExtents[0] * outputExtents[1] + outputY * outputExtents[0] + outputX;
                		/* if color: */
                		if ( (bufferType == DataType.ARGB) || (bufferType == DataType.ARGB_USHORT)
                				|| (bufferType == DataType.ARGB_FLOAT)) {
                			output.set( (outputIndex * 4) + 0, original.getFloat( (inputIndex * 4) + 0));
                			output.set( (outputIndex * 4) + 1, original.getFloat( (inputIndex * 4) + 1));
                			output.set( (outputIndex * 4) + 2, original.getFloat( (inputIndex * 4) + 2));
                			output.set( (outputIndex * 4) + 3, original.getFloat( (inputIndex * 4) + 3));
                		}
                		/* not color: */
                		else {
                			output.set( outputIndex, original.getFloat(inputIndex));
                		}
    				}
    			}
    		}
    	}

    	output.calcMinMax();
//    	original.calcMinMax();
//    	transform.calcMinMax();
    	new ViewJFrameImage(output);
//    	new ViewJFrameImage(original);
//    	new ViewJFrameImage(transform);
    }
    
    private void updateLattice( boolean rebuild )
	{
    	if ( left == null || right == null )
    	{
    		return;
    	}
    	if ( right.size() == 0 )
    	{
    		return;
    	}
		if ( rebuild )
		{
//			System.err.println( "new pt added" );
			if ( latticeGrid != null )
			{
				for ( int i = latticeGrid.size() - 1; i >= 0; i-- )
				{
					VOI marker = latticeGrid.remove(i);
					imageA.unregisterVOI( marker );
				}
			}
			else
			{
				latticeGrid = new VOIVector();
			}
			for ( int j = 0; j < Math.min(left.size(), right.size()); j++ )
			{
				short id = (short) imageA.getVOIs().getUniqueID();
				VOI marker = new VOI(id, "pair_" + j, VOI.POLYLINE, (float)Math.random() );
				VOIContour mainAxis = new VOIContour(false); 		    		    		
				mainAxis.add( left.elementAt(j) );
				mainAxis.add( right.elementAt(j) );
				marker.getCurves().add(mainAxis);
				marker.setColor( new Color( 255, 255, 0) );
				mainAxis.update( new ColorRGBA(1,1,0,1));
				if ( j == 0 )
				{
					marker.setColor( new Color( 0, 255, 0) );
					mainAxis.update( new ColorRGBA(0,1,0,1));
				}
				imageA.registerVOI( marker );
				latticeGrid.add(marker);
			}			
		}
		else
		{
			for ( int i = 0; i < latticeGrid.size(); i++ )
			{
				VOI marker = latticeGrid.elementAt(i);
				marker.getCurves().elementAt(0).elementAt(0).copy( left.elementAt(i) );
				marker.getCurves().elementAt(0).elementAt(1).copy( right.elementAt(i) );
				marker.update();
			}
		}
		left.update();
		right.update();
		

		if ( centerLine != null )
		{
			imageA.unregisterVOI( centerLine );
		}
		if ( rightLine != null )
		{
			imageA.unregisterVOI( rightLine );
		}
		if ( leftLine != null )
		{
			imageA.unregisterVOI( leftLine );
		}
		boolean showContours = false;
		if ( displayContours != null )
		{
			showContours = (imageA.isRegistered( displayContours ) != -1);
			if ( showContours )
			{
				imageA.unregisterVOI( displayContours );				
			}
		}

    	if ( (left.size() == right.size()) && (left.size() >= 2) )
    	{
    		generateCurves();
    		if ( showContours )
    		{
    			imageA.registerVOI( displayContours );
    		}
    	}
        
    	updateSelected();
        
        // when everything's done, notify the image listeners
        imageA.notifyImageDisplayListeners();
	}
    
    private void updateLinks()
    {
		if ( latticeGrid != null )
		{
			latticeGrid.clear();
		}
		else
		{
			latticeGrid = new VOIVector();
		}
		
		annotationVOIs = null;
		leftMarker = null;
		rightMarker = null;
    	VOIVector vois = imageA.getVOIs();
    	for ( int i = 0; i < vois.size(); i++ )
    	{
    		VOI voi = vois.elementAt(i);
    		String name = voi.getName();
//    		System.err.println( vois.elementAt(i).getName() );
    		if ( name.equals("lattice") )
    		{
    			lattice = voi;
    			left = (VOIContour) lattice.getCurves().elementAt(0);
    			right = (VOIContour) lattice.getCurves().elementAt(1);    		
    		}
    		else if ( name.equals("left line") )
    		{
    			leftLine = voi;
    		}
    		else if ( name.equals("right line") )
    		{
    			rightLine = voi;
    		}
    		else if ( name.equals("center line") )
    		{
    			centerLine = voi;
    		}
    		else if ( name.contains("pair_") )
    		{
    			latticeGrid.add(voi);
    		}
    		else if ( name.contains("wormContours") )
    		{
    			displayContours = voi;
    		}
    		else if ( name.contains("interpolatedContours") )
    		{
    			displayInterpolatedContours = voi;
    		}
    		else if ( name.equals("showSelected") )
    		{
    			showSelectedVOI = voi;
//    			System.err.println("updateLinks showSelected ");
    		}
    		else if ( name.equals("leftMarker") )
    		{
    			leftMarker = voi;
//    			System.err.println("updateLinks showSelected ");
    		}
    		else if ( name.equals("rightMarker") )
    		{
    			rightMarker = voi;
//    			System.err.println("updateLinks showSelected ");
    		}
    		else if ( name.equals("annotationVOIs") )
    		{
    			annotationVOIs = voi;
    		}
    	}
    	clear3DSelection();
    	if ( showSelected != null )
    	{
    		for (int i = 0; i < showSelected.length; i++ )
    		{
    			showSelected[i].dispose();
    		}
    		showSelected = null;
    	}
    	showSelectedVOI = null;
    	updateLattice(true);
    }
    
    private void updateSelected()
    {
        if ( pickedPoint != null )
        {
        	if ( showSelectedVOI == null )
        	{
				short id = (short) imageA.getVOIs().getUniqueID();
				showSelectedVOI = new VOI(id, "showSelected", VOI.POLYLINE, (float)Math.random() );
				imageA.registerVOI(showSelectedVOI);
        	}
        	if ( showSelected == null )
        	{
        		showSelected = new VOIContour[3];
        		showSelected[0] = new VOIContour(true);
        		makeEllipse( Vector3f.UNIT_X, Vector3f.UNIT_Y, pickedPoint, 4, showSelected[0] );
        		showSelectedVOI.getCurves().add(showSelected[0]);
        		showSelected[0].update( new ColorRGBA(0,1,1,1));
        		

        		showSelected[1] = new VOIContour(true);
        		makeEllipse( Vector3f.UNIT_Z, Vector3f.UNIT_Y, pickedPoint, 4, showSelected[1] );
        		showSelectedVOI.getCurves().add(showSelected[1]);
        		showSelected[1].update( new ColorRGBA(0,1,1,1));
        		

        		showSelected[2] = new VOIContour(true);
        		makeEllipse( Vector3f.UNIT_Z, Vector3f.UNIT_X, pickedPoint, 4, showSelected[2] );
        		showSelectedVOI.getCurves().add(showSelected[2]);
        		showSelected[2].update( new ColorRGBA(0,1,1,1));
        		
    			showSelectedVOI.setColor( new Color( 0, 255, 255) );
        	}
        	else
        	{
        		for (int i = 0; i < showSelected.length; i++ )
        		{
        			Vector3f center = new Vector3f();
        			for ( int j = 0; j < showSelected[i].size(); j++ )
        			{
        				center.add(showSelected[i].elementAt(j) );
        			}
        			center.scale(1f/showSelected[i].size());
        			Vector3f diff = Vector3f.sub( pickedPoint, center );
        			for ( int j = 0; j < showSelected[i].size(); j++ )
        			{
        				showSelected[i].elementAt(j).add( diff );
        			}
        		}
    			showSelectedVOI.update();
        	}
			if ( imageA.isRegistered( showSelectedVOI ) == -1 )
			{
				imageA.registerVOI(showSelectedVOI);
			}
        }        
    }
    
//    private VOIContour writeDiagonal( ModelImage model, final int tSlice, final int slice, final int[] extents, final Vector3f[] verts) 
//    {
//        final int iBound = extents[0];
//        final int jBound = extents[1];
//        int[] dimExtents = model.getExtents();
//        
//        /*
//         * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
//         * coordinate-systems: transformation:
//         */
//        final int iFactor = 1;
//        final int jFactor = dimExtents[0];
//        final int kFactor = dimExtents[0] * dimExtents[1];
//        final int tFactor = dimExtents[0] * dimExtents[1] * dimExtents[2];
//
//        int buffFactor = 1;
//        
//        Vector3f center = new Vector3f();
//        for ( int i = 0; i < verts.length; i++ )
//        {
//        	center.add(verts[i]);
//        }
//        center.scale( 1f/verts.length );
//        
//        /* Calculate the slopes for traversing the data in x,y,z: */
//        float xSlopeX = verts[1].X - verts[0].X;
//        float ySlopeX = verts[1].Y - verts[0].Y;
//        float zSlopeX = verts[1].Z - verts[0].Z;
//
//        float xSlopeY = verts[3].X - verts[0].X;
//        float ySlopeY = verts[3].Y - verts[0].Y;
//        float zSlopeY = verts[3].Z - verts[0].Z;
//
//        float x0 = verts[0].X;
//        float y0 = verts[0].Y;
//        float z0 = verts[0].Z;
//
//        xSlopeX /= (iBound - 1);
//        ySlopeX /= (iBound - 1);
//        zSlopeX /= (iBound - 1);
//
//        xSlopeY /= (jBound - 1);
//        ySlopeY /= (jBound - 1);
//        zSlopeY /= (jBound - 1);
//
//        /* loop over the 2D image (values) we're writing into */
//        float x = x0;
//        float y = y0;
//        float z = z0;
//
//        VOIContour insideList = new VOIContour(false);
//        for (int j = 0; j < jBound; j++) {
//
//            /* Initialize the first diagonal point(x,y,z): */
//            x = x0;
//            y = y0;
//            z = z0;
//
//            for (int i = 0; i < iBound; i++) {
//                final int iIndex = Math.round(x);
//                final int jIndex = Math.round(y);
//                final int kIndex = Math.round(z);
//
//                /* calculate the ModelImage space index: */
//                final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));
//
//                // Bounds checking:
//                if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
//                		|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > model.getSize()))) {
//
//                	// do nothing
//                } else {
//                	if ( model != null )
//                	{
//                		float currentValue = model.getFloatTriLinearBounds(x, y, z);
//                		if ( Math.abs(currentValue - (slice+1)) < SampleLimit )
//                		{
//                			insideList.add( new Vector3f(x,y,z) );
//                		}
//                	}
//                }
//                /*
//                 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
//                 * ySlopeX and zSlopeX values:
//                 */
//                x = x + xSlopeX;
//                y = y + ySlopeX;
//                z = z + zSlopeX;
//            }
//
//            /*
//             * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
//             * ySlopeY and zSlopeY values:
//             */
//            x0 = x0 + xSlopeY;
//            y0 = y0 + ySlopeY;
//            z0 = z0 + zSlopeY;
//        }
//        return insideList;
//    }
    
    private void writeDiagonal( ModelImage image, ModelImage model, ModelImage originToStraight, final int tSlice, final int slice, final int[] extents,
            final Vector3f[] verts, final float[] values, float[] dataOrigin, float[] sampleDistance, float[] sampleDistanceP) 
    {
        final int iBound = extents[0];
        final int jBound = extents[1];
        int[] dimExtents = image.getExtents();
        
        /*
         * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
         * coordinate-systems: transformation:
         */
        final int iFactor = 1;
        final int jFactor = dimExtents[0];
        final int kFactor = dimExtents[0] * dimExtents[1];
        final int tFactor = dimExtents[0] * dimExtents[1] * dimExtents[2];

        int buffFactor = 1;

        if ( (image.getType() == ModelStorageBase.ARGB) || (image.getType() == ModelStorageBase.ARGB_USHORT)
                || (image.getType() == ModelStorageBase.ARGB_FLOAT)) {
            buffFactor = 4;
        }
        
        Vector3f center = new Vector3f();
        for ( int i = 0; i < verts.length; i++ )
        {
        	center.add(verts[i]);
        }
        center.scale( 1f/verts.length );
        
        /* Calculate the slopes for traversing the data in x,y,z: */
        float xSlopeX = verts[1].X - verts[0].X;
        float ySlopeX = verts[1].Y - verts[0].Y;
        float zSlopeX = verts[1].Z - verts[0].Z;

        float xSlopeY = verts[3].X - verts[0].X;
        float ySlopeY = verts[3].Y - verts[0].Y;
        float zSlopeY = verts[3].Z - verts[0].Z;

        float x0 = verts[0].X;
        float y0 = verts[0].Y;
        float z0 = verts[0].Z;

        xSlopeX /= (iBound);
        ySlopeX /= (iBound);
        zSlopeX /= (iBound);

        xSlopeY /= (jBound);
        ySlopeY /= (jBound);
        zSlopeY /= (jBound);

        /* loop over the 2D image (values) we're writing into */
        float x = x0;
        float y = y0;
        float z = z0;

        for (int j = 0; j < jBound; j++) {

            /* Initialize the first diagonal point(x,y,z): */
            x = x0;
            y = y0;
            z = z0;

            for (int i = 0; i < iBound; i++) {
                final int iIndex = Math.round(x);
                final int jIndex = Math.round(y);
                final int kIndex = Math.round(z);

                /* calculate the ModelImage space index: */
                final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));

                if ( sampleDistance != null )
                {
                	sampleDistance[(j * iBound) + i] = 0;
                }
    			
                // Bounds checking:
                if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
                		|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > image.getSize()))) {

                	// do nothing
                } else {
                	float currentValue = (slice+1);
                	if ( model != null )
                	{
//                		currentValue = model.getFloat((int)x, (int)y, (int)z);
//                		currentValue = model.getFloatTriLinearBounds(x, y, z);
                		currentValue = model.getFloat(iIndex, jIndex, kIndex);
                	}
                	if ( currentValue == 0 )
                	{
                    	if ( buffFactor == 4 ) {
                    		values[ ( ( (j * iBound) + i) * 4) + 0] = Math.max((float)image.getMinA(), values[ ( ( (j * iBound) + i) * 4) + 0]);
                    		values[ ( ( (j * iBound) + i) * 4) + 1] = Math.max((float)image.getMinR(), values[ ( ( (j * iBound) + i) * 4) + 1]);
                    		values[ ( ( (j * iBound) + i) * 4) + 2] = Math.max((float)image.getMinG(), values[ ( ( (j * iBound) + i) * 4) + 2]);
                    		values[ ( ( (j * iBound) + i) * 4) + 3] = Math.max((float)image.getMinB(), values[ ( ( (j * iBound) + i) * 4) + 3]);
                    	}
                    	/* not color: */
                    	else {
                    		values[ (j * iBound) + i] = Math.max((float)image.getMin(), values[ (j * iBound) + i]);
                    	}                		
                	}
                	else if ( Math.abs(currentValue - (slice+1)) < SampleLimit )
                	{
                    	/* if color: */
                    	if ( buffFactor == 4 ) {
                    		float tempV =  Math.max(image.getFloatC( iIndex, jIndex, kIndex, 1),image.getFloatC( iIndex, jIndex, kIndex, 2));
                    		if ( (tempV > values[ ( ( (j * iBound) + i) * 4) + 1]) || (tempV > values[ ( ( (j * iBound) + i) * 4) + 2]) )
                    		{
                    			values[ ( ( (j * iBound) + i) * 4) + 0] = image.getFloatC( iIndex, jIndex, kIndex, 0);
                    			values[ ( ( (j * iBound) + i) * 4) + 1] = image.getFloatC( iIndex, jIndex, kIndex, 1);
                    			values[ ( ( (j * iBound) + i) * 4) + 2] = image.getFloatC( iIndex, jIndex, kIndex, 2);
                    			values[ ( ( (j * iBound) + i) * 4) + 3] = image.getFloatC( iIndex, jIndex, kIndex, 3);
                    			if ( dataOrigin != null )
                    			{
                    				dataOrigin[ ( ( (j * iBound) + i) * 4) + 0] = 1;
                    				dataOrigin[ ( ( (j * iBound) + i) * 4) + 1] = x;
                    				dataOrigin[ ( ( (j * iBound) + i) * 4) + 2] = y;
                    				dataOrigin[ ( ( (j * iBound) + i) * 4) + 3] = z;
                    			}
                    		}
                    	}
                    	/* not color: */
                    	else {
                    		float tempV = image.getFloat(iIndex, jIndex, kIndex);
//                    		if ( tempV > values[ (j * iBound) + i] )
//                    		{
//                    			values[ (j * iBound) + i] = tempV;
//                            	if ( dataOrigin != null )
//                            	{
//                            		dataOrigin[ ( ( (j * iBound) + i) * 4) + 0] = 1;
//                            		dataOrigin[ ( ( (j * iBound) + i) * 4) + 1] = x;
//                            		dataOrigin[ ( ( (j * iBound) + i) * 4) + 2] = y;
//                            		dataOrigin[ ( ( (j * iBound) + i) * 4) + 3] = z;
//                            	}
//                    		}
                			values[ (j * iBound) + i] += tempV;
                        	if ( dataOrigin != null )
                        	{
                        		dataOrigin[ ( ( (j * iBound) + i) * 4) + 0] = 1;
                        		dataOrigin[ ( ( (j * iBound) + i) * 4) + 1] += x;
                        		dataOrigin[ ( ( (j * iBound) + i) * 4) + 2] += y;
                        		dataOrigin[ ( ( (j * iBound) + i) * 4) + 3] += z;
                        	}
                    	}

                    	if ( (sampleDistanceP != null) && (sampleDistance != null) )
                    	{
                    		if ( (sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 1] != 0) &&
                    			 (sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 2] != 0) &&
                    			 (sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 3] != 0) 	)
                    		{
                    			float distance = (x - sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 1]) * (x - sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 1]) +
                    					(y - sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 2]) * (y - sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 2]) +
                    					(z - sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 3]) * (z - sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 3]);
                    			distance = (float) Math.sqrt(distance);
                    			sampleDistance[(j * iBound) + i] = distance;
                    		}
                    	}

                    	if ( originToStraight != null )
                    	{
                    		originToStraight.setC( iIndex, jIndex, kIndex, 0, 1 );
                    		originToStraight.setC( iIndex, jIndex, kIndex, 1, i );
                    		originToStraight.setC( iIndex, jIndex, kIndex, 2, j );
                    		originToStraight.setC( iIndex, jIndex, kIndex, 3, slice );
                    	}
                	}
                }
                if ( sampleDistanceP != null)
                {
                	sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 0] = 1;
                	sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 1] = x;
                	sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 2] = y;
                	sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 3] = z;
                }
                /*
                 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
                 * ySlopeX and zSlopeX values:
                 */
                x = x + xSlopeX;
                y = y + ySlopeX;
                z = z + zSlopeX;
            }

            /*
             * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
             * ySlopeY and zSlopeY values:
             */
            x0 = x0 + xSlopeY;
            y0 = y0 + ySlopeY;
            z0 = z0 + zSlopeY;
        }
        
        if ( (xSlopeX > 1) || (ySlopeX > 1) || (zSlopeX > 1) || (xSlopeY > 1) || (ySlopeY > 1) || (zSlopeY > 1) )
        {
        	System.err.println( "writeDiagonal " + xSlopeX + " " + ySlopeX + " " + zSlopeX );
        	System.err.println( "writeDiagonal " + xSlopeY + " " + ySlopeY + " " + zSlopeY );
        }
        	
        	
        
//        checkConvex( values, dataOrigin, (float)image.getMin(), outsideVal, buffFactor, iBound, jBound, iBoundHalf, jBoundHalf );
    }
    
}
