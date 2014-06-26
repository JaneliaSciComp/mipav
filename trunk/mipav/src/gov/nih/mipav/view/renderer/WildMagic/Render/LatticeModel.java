package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.ModelStorageBase.DataType;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.ViewVOIVector;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.Interface.FileSurface_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JDialogLattice;

import java.awt.Color;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Vector;

import javax.swing.JFileChooser;

import WildMagic.LibFoundation.Curves.NaturalSpline3;
import WildMagic.LibFoundation.Distance.DistanceSegment3Segment3;
import WildMagic.LibFoundation.Distance.DistanceVector3Segment3;
import WildMagic.LibFoundation.Intersection.IntrTriangle3Triangle3f;
import WildMagic.LibFoundation.Mathematics.Box3f;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Ellipsoid3f;
import WildMagic.LibFoundation.Mathematics.Segment3f;
import WildMagic.LibFoundation.Mathematics.Triangle3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
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
	
	private float minCurve = Float.MAX_VALUE;	
	private float maxCurve = -Float.MAX_VALUE;

    private Vector<Vector3f> centerTangents;
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
	private VOI showSelectedVOI = null;
	private VOIContour[] showSelected = null;
	private int[] latticeSlice;
	private int DiameterBuffer = 0;
	private int SampleLimit = 10;
	private	float minRange = .025f;

	
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

		if ( centerTangents != null )
			centerTangents.clear();
		centerTangents = null;
		
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
		latticeSlice = null;		
	}
	
	public void addInsertionPoint( Vector3f startPt, Vector3f endPt, Vector3f maxPt )
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
    	if ( minIndexL != -1 )
    	{
//			System.err.println( "Add to left " + (minIndexL+1) );
			left.add( minIndexL + 1, newLeft );
			pickedPoint = left.elementAt(minIndexL+1);
			newRight = Vector3f.add( right.elementAt(minIndexL), right.elementAt(minIndexL+1) );
			newRight.scale(0.5f);
			right.add( minIndexL + 1, newRight );
			
        	updateLattice(true);
    	}
    	if ( minIndexR != -1 )
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
    
    public void clear3DSelection()
    {
    	pickedPoint = null;
    	if ( showSelected != null )
    	{
    		imageA.unregisterVOI(showSelectedVOI);
    	}
    }
    


    public void interpolate( ModelImage srcImage, ModelImage destImage, VOI samplingPlanes, Vector<Ellipsoid3f> ellipseBounds, int diameter )
    {
		int dimX = srcImage.getExtents().length > 0 ? srcImage.getExtents()[0] : 1;
		int dimY = srcImage.getExtents().length > 1 ? srcImage.getExtents()[1] : 1;
		int[] resultExtents = new int[]{dimX, dimY, samplingPlanes.getCurves().size()};

		float[] values = new float[dimX*dimY];		
		
		System.err.println( dimX + "  ==?   " + diameter );
		
		System.err.println( srcImage.getExtents()[2] + "  ==?   " + samplingPlanes.getCurves().size() );
		int end = Math.min( srcImage.getExtents()[2], samplingPlanes.getCurves().size() );
		for( int i = 0; i < end; i++ )
		{
//			float diameterInterp = samplingDiameters.elementAt(i);
			VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
	        Vector3f[] corners = new Vector3f[4];
	        for ( int j = 0; j < 4; j++ )
	        {
	        	corners[j] = kBox.elementAt(j);
	        }
			try {
				srcImage.exportData( i * values.length, values.length, values );
				destImage.writeDiagonal( 0, i, resultExtents, corners, ellipseBounds.elementAt(i), values );
			} catch(IOException e) {
				e.printStackTrace();
			}
		}
		
		destImage.calcMinMax();
    }
    

    public void interpolateImages( ModelImage srcImage, ModelImage destImage, VOI destLattice )
	{

		// Assume image is isotropic (square voxels).
		if ( destLattice.getCurves().size() != 2 )
		{
			return;
		}
		VOIContour left = (VOIContour) destLattice.getCurves().elementAt(0);
		VOIContour right = (VOIContour) destLattice.getCurves().elementAt(1);
		if ( left.size() != right.size() )
		{
			return;
		}
		VOIContour center = new VOIContour(false);
		for ( int i = 0; i < left.size(); i++ )
		{
			Vector3f centerPt = Vector3f.add(left.elementAt(i), right.elementAt(i) );
			centerPt.scale(0.5f);
			center.add(centerPt);
		}

    	float[] afTimeC = new float[center.size()];
		NaturalSpline3 centerSpline = smoothCurve(center, afTimeC);
		NaturalSpline3 leftSpline = smoothCurve2(left, afTimeC);
		NaturalSpline3 rightSpline = smoothCurve2(right, afTimeC);

		
		Vector<Vector3f> centerPositions = new Vector<Vector3f>();
		Vector<Vector3f> centerTangents = new Vector<Vector3f>();
		Vector<Float> wormDiameters = new Vector<Float>();
		Vector<Vector3f> rightVectors = new Vector<Vector3f>();
		Vector<Vector3f> upVectors = new Vector<Vector3f>();
		
		float length = centerSpline.GetLength(0, 1);
		int srcLength = srcImage.getExtents()[2];
		int extent = 0;
		float[] allTimes = new float[srcLength];
		float minCurve = Float.MAX_VALUE;
		float maxCurve = -Float.MAX_VALUE;
		for ( int i = 0; i < srcLength; i++ )
		{
			int timeIndex = (int) ((length-1) * i/(srcLength - 1));
//			System.err.println( i + " " + timeIndex );
			float t = centerSpline.GetTime(timeIndex);
			allTimes[i] = t;
			centerPositions.add(centerSpline.GetPosition(t));
			centerTangents.add( centerSpline.GetFirstDerivative(t) );
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
			
			centerTangents.elementAt(i).normalize();
			Vector3f upDir = Vector3f.cross( rightDir, centerTangents.elementAt(i) );
			upDir.normalize();
			upVectors.add(upDir);
//			if ( i > 0 )
//			{
//				System.err.println( i + "   " + centerPositions.elementAt(i).distance(centerPositions.elementAt(i-1)));
//			}
			float curve = centerSpline.GetSecondDerivative(t).length();
        	if ( curve < minCurve )
        	{
        		minCurve = curve;
        	}
        	if ( curve > maxCurve )
        	{
        		maxCurve = curve;
        	}

		}		
		extent += 10;
		
		Vector<Ellipsoid3f> ellipseBounds = new Vector<Ellipsoid3f>();
		short sID = (short)(destImage.getVOIs().getUniqueID());
		VOI samplingPlanes = new VOI(sID, "samplingPlanes");
		VOI wormContours = new VOI(sID, "wormContours");
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

	        float curve = centerSpline.GetSecondDerivative(allTimes[i]).length();
	        float scale = (curve - minCurve)/(maxCurve - minCurve);
	        VOIContour ellipse = new VOIContour(true);
	        Ellipsoid3f ellipsoid = makeEllipse( rkRVector, rkUVector, rkEye, wormDiameters.elementAt(i), scale, ellipse );
//	        System.err.println( i + " " + rkEye );
	        ellipseBounds.add( ellipsoid );
	        wormContours.importCurve(ellipse);

	        VOIContour kBox = new VOIContour(true);
			for ( int j = 0; j < 4; j++ )
			{
				kBox.addElement( output[j].X, output[j].Y, output[j].Z );
			}
//			System.err.println( kBox.elementAt(0).distance( kBox.elementAt(1) ) + " " + kBox.elementAt(2).distance( kBox.elementAt(3) ) );
//			System.err.println( kBox.elementAt(0).distance( kBox.elementAt(3) ) + " " + kBox.elementAt(1).distance( kBox.elementAt(2) ) );
			kBox.update( new ColorRGBA(0,0,1,1) );		
//	        if ( (i%40) == 0 )
	        {	
	        	samplingPlanes.importCurve(kBox);
	        }
		}
		VOIContour centerLine = new VOIContour(false);
		centerLine.addAll( centerPositions );
		sID = (short)(destImage.getVOIs().getUniqueID());
		VOI samplingPoints = new VOI(sID, "samplingPlanes");
		samplingPoints.getCurves().add(centerLine);
		destImage.registerVOI(samplingPoints);
		
		interpolate(srcImage, destImage, samplingPlanes, ellipseBounds, 2*extent );
	}
       
    
    public void showInterpolatedModel()
    {
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
	        

	        float curve = centerSpline.GetSecondDerivative(allTimes[i]).length();
	        float scale = (curve - minCurve)/(maxCurve - minCurve);
	        VOIContour ellipse = new VOIContour(true);
	        Ellipsoid3f ellipsoid = makeEllipse( rkRVector, rkUVector, rkEye, wormDiameters.elementAt(i), scale, ellipse );
	        ellipseBounds.add(ellipsoid);
	        
	        
	        Box3f box = new Box3f( ellipsoid.Center, ellipsoid.Axis, new float[]{extent, extent, 1 } );
	        boxBounds.add(box);
		}
		generateMasks( imageA, imageB, samplingPlanes, ellipseBounds, wormDiameters, 2*extent, false, false  );
    }
    
    
    public ModelImage interpolateLattice( int version, boolean displayResult )
	{
		latticeSlice = new int[afTimeC.length];
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
					latticeSlice[i] = j;
					closestTimes[i] = allTimes[j];
				}
			}
			leftDistances[i] = 0;
			rightDistances[i] = 0;
			if ( i > 0 )
			{
				float curveDistance = 0;
				for ( int j = latticeSlice[i-1]+1; j <= latticeSlice[i]; j++ )
				{
					curveDistance += centerSpline.GetPosition(allTimes[j]).distance( centerSpline.GetPosition(allTimes[j-1]) );
				}
//				System.err.println( i + "   " + curveDistance);
//				System.err.println( i + "   " + (latticeSlice[i] - latticeSlice[i-1]) );


				curveDistance = 0;
				for ( int j = latticeSlice[i-1]+1; j <= latticeSlice[i]; j++ )
				{
					curveDistance += leftSpline.GetPosition(allTimes[j]).distance( leftSpline.GetPosition(allTimes[j-1]) );
				}
				leftDistances[i] = curveDistance;

				curveDistance = 0;
				for ( int j = latticeSlice[i-1]+1; j <= latticeSlice[i]; j++ )
				{
					curveDistance += rightSpline.GetPosition(allTimes[j]).distance( rightSpline.GetPosition(allTimes[j-1]) );
				}
				rightDistances[i] = curveDistance;
			}
		}

		saveLatticeStatistics(imageA, length, left, right, leftDistances, rightDistances, "_before");
		
		
		
		
		


		boxBounds = new Vector<Box3f>();
		ellipseBounds = new Vector<Ellipsoid3f>();
		short sID = (short)(imageA.getVOIs().getUniqueID());
		samplingPlanes = new VOI(sID, "samplingPlanes");
//		displayContours = new VOI(sID, "wormContours");
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
	        

	        float curve = centerSpline.GetSecondDerivative(allTimes[i]).length();
	        float scale = (curve - minCurve)/(maxCurve - minCurve);
	        VOIContour ellipse = new VOIContour(true);
	        Ellipsoid3f ellipsoid = makeEllipse( rkRVector, rkUVector, rkEye, wormDiameters.elementAt(i), scale, ellipse );
	        ellipseBounds.add(ellipsoid);
	        
	        
	        Box3f box = new Box3f( ellipsoid.Center, ellipsoid.Axis, new float[]{extent, extent, 1 } );
	        boxBounds.add(box);
		}
						
		ModelImage straightImage = null;
		if ( version == 2 )
		{
			generateMasks( imageA, imageB, samplingPlanes, ellipseBounds, wormDiameters, 2*extent, true, displayResult  );
		}
		else if ( version == 0 )
		{
			straightImage = straighten(imageA, samplingPlanes, ellipseBounds, 2*extent, latticeSlice, true );
			if ( imageB != null )
			{
				straighten(imageB, samplingPlanes, ellipseBounds, 2*extent, latticeSlice, false );
			}
		}

		return straightImage;
	}
    
    
    
    public Ellipsoid3f makeEllipse( Vector3f right, Vector3f up, Vector3f center, float diameterA, float scale, VOIContour ellipse  )
	{
		int numPts = 32;
		double[] adCos = new double[32];
		double[] adSin = new double[32];
		for ( int i = 0; i < numPts; i++ )
		{
			adCos[i] = Math.cos( Math.PI * 2.0 * i/numPts );
			adSin[i] = Math.sin( Math.PI * 2.0 * i/numPts);
		}
		float diameterB = diameterA/2f + (1-scale) * diameterA/2f;
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
//		System.err.println( diameterA + "   " + diameterB + "   " + 1 );
		return new Ellipsoid3f( center, axes, extents );
	}
    
    public void makeEllipse( Vector3f right, Vector3f up, Vector3f center, float diameter, VOIContour ellipse  )
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
    
    public TriMesh makeModel( boolean fullModel )
	{						
		float scale = 1;
		int numPts = 6;
		double[] adCos = new double[numPts];
		double[] adSin = new double[numPts];
		for ( int i = 0; i < numPts; i++ )
		{
			adCos[i] = Math.cos( Math.PI * 2.0 * i/numPts );
			adSin[i] = Math.sin( Math.PI * 2.0 * i/numPts);
		}
		
		int numSlices = left.size();
		if ( fullModel )
		{
			numSlices = centerPositions.size();
		}
		Attributes attributes = new Attributes();
		attributes.SetPChannels(3);
		attributes.SetNChannels(3);
		attributes.SetCChannels(0,3);
		attributes.SetTChannels(0, 2);
		VertexBuffer vBuffer = new VertexBuffer( attributes, numSlices * numPts + 2 );
		IndexBuffer iBuffer = new IndexBuffer( (vBuffer.GetVertexQuantity() - 2 ) * 6 );
		int vCount = 0;
		int nCount = 0;
		int cCount = 0;
		int iCount = 0;
		int tCount = 0;
		int gCount = 0;
		int z = 0;
		
    	boolean[] grow = new boolean[vBuffer.GetVertexQuantity() ];
		for ( int i = 0; i < numSlices; i++ )
		{
			float tx = (z++ + (numSlices/2))/(float)(numSlices-1);
			int index = i;
			if ( !fullModel )
			{
				index = latticeSlice[i];
			}
			
			
			Vector3f rkEye = centerPositions.elementAt(index);
			Vector3f rkRVector = rightVectors.elementAt(index);
			Vector3f rkUVector = upVectors.elementAt(index);
	        Vector3f rkDVector = centerTangents.elementAt(index);
	        scale = wormDiameters.elementAt(index);
			if ( i == 0 )
			{
				vBuffer.SetPosition3(vCount++, rkEye);
				vBuffer.SetNormal3(nCount++, Vector3f.neg(rkDVector) );
				vBuffer.SetColor4(0, cCount++, 0, 0, 1, 1 );
				vBuffer.SetTCoord2(0, tCount++, tx, 0 );
				grow[gCount++] = false;
				for ( int j = 0; j < numPts; j++ )
				{
					Vector3f pos1 = Vector3f.scale((float) (scale * adCos[j]), rkRVector);
					Vector3f pos2 = Vector3f.scale((float) (scale * adSin[j]), rkUVector);
					Vector3f pos = Vector3f.add(pos1,pos2);
					pos.add(rkEye);
					Vector3f normal = Vector3f.sub( pos, rkEye );
					normal.normalize();
					vBuffer.SetPosition3(vCount++, pos);
					vBuffer.SetNormal3(nCount++, normal );
					vBuffer.SetTCoord2(0, tCount++, tx, j / (float)(numPts-1) );
					vBuffer.SetColor4(0, cCount++, 0, 0, 1, 1 );
					grow[gCount++] = true;
					
					if ( j < (numPts-1) )
					{
						iBuffer.GetData()[iCount++] = vCount - 1;
						iBuffer.GetData()[iCount++] = vCount;
						iBuffer.GetData()[iCount++] = 0;
						
						iBuffer.GetData()[iCount++] = vCount;
						iBuffer.GetData()[iCount++] = vCount - 1;
						iBuffer.GetData()[iCount++] = vCount - 1 + numPts;

						iBuffer.GetData()[iCount++] = vCount;
						iBuffer.GetData()[iCount++] = vCount - 1 + numPts;
						iBuffer.GetData()[iCount++] = vCount + numPts;
					}
					else
					{
						iBuffer.GetData()[iCount++] = vCount - 1;
						iBuffer.GetData()[iCount++] = vCount - numPts;
						iBuffer.GetData()[iCount++] = 0;
						
						iBuffer.GetData()[iCount++] = vCount - numPts;
						iBuffer.GetData()[iCount++] = vCount - 1;
						iBuffer.GetData()[iCount++] = vCount - 1 + numPts;

						iBuffer.GetData()[iCount++] = vCount - numPts;
						iBuffer.GetData()[iCount++] = vCount - 1 + numPts;
						iBuffer.GetData()[iCount++] = vCount;
					}
				}
			}
			else if ( i == (numSlices - 1) )
			{				
				int centerIndex = vBuffer.GetVertexQuantity() - 1;
				for ( int j = 0; j < numPts; j++ )
				{
					Vector3f pos1 = Vector3f.scale((float) (scale * adCos[j]), rkRVector);
					Vector3f pos2 = Vector3f.scale((float) (scale * adSin[j]), rkUVector);
					Vector3f pos = Vector3f.add(pos1,pos2);
					pos.add(rkEye);
					Vector3f normal = Vector3f.sub( pos, rkEye );
					normal.normalize();
					vBuffer.SetPosition3(vCount++, pos);
					vBuffer.SetNormal3(nCount++, normal );
					vBuffer.SetTCoord2(0, tCount++, tx, j / (float)(numPts-1) );
					vBuffer.SetColor4(0, cCount++, 0, 0, 1, 1 );
					grow[gCount++] = true;
					
					if ( j < (numPts-1) )
					{
						iBuffer.GetData()[iCount++] = vCount;
						iBuffer.GetData()[iCount++] = vCount - 1;
						iBuffer.GetData()[iCount++] = centerIndex;
					}
					else
					{
						iBuffer.GetData()[iCount++] = vCount - numPts;
						iBuffer.GetData()[iCount++] = vCount - 1;
						iBuffer.GetData()[iCount++] = centerIndex;
					}
				}

				vBuffer.SetPosition3(vCount++, rkEye);
				vBuffer.SetNormal3(nCount++, rkDVector );
				grow[gCount++] = false;
			}
			else
			{				
				for ( int j = 0; j < numPts; j++ )
				{
					Vector3f pos1 = Vector3f.scale((float) (scale * adCos[j]), rkRVector);
					Vector3f pos2 = Vector3f.scale((float) (scale * adSin[j]), rkUVector);
					Vector3f pos = Vector3f.add(pos1,pos2);
					pos.add(rkEye);
					Vector3f normal = Vector3f.sub( pos, rkEye );
					normal.normalize();
					vBuffer.SetPosition3(vCount++, pos);
					vBuffer.SetNormal3(nCount++, normal );
					vBuffer.SetTCoord2(0, tCount++, tx, j / (float)(numPts-1) );
					vBuffer.SetColor4(0, cCount++, 0, 0, 1, 1 );
					grow[gCount++] = true;
					
					if ( j < (numPts-1) )
					{
						iBuffer.GetData()[iCount++] = vCount;
						iBuffer.GetData()[iCount++] = vCount - 1;
						iBuffer.GetData()[iCount++] = vCount - 1 + numPts;

						iBuffer.GetData()[iCount++] = vCount;
						iBuffer.GetData()[iCount++] = vCount - 1 + numPts;
						iBuffer.GetData()[iCount++] = vCount + numPts;
					}
					else
					{
						iBuffer.GetData()[iCount++] = vCount - numPts;
						iBuffer.GetData()[iCount++] = vCount - 1;
						iBuffer.GetData()[iCount++] = vCount - 1 + numPts;

						iBuffer.GetData()[iCount++] = vCount - numPts;
						iBuffer.GetData()[iCount++] = vCount - 1 + numPts;
						iBuffer.GetData()[iCount++] = vCount;
					}
				}
			}
		}
		System.err.println( iCount + " " + iBuffer.GetData().length );
		System.err.println( vCount + " " + vBuffer.GetVertexQuantity() );
		TriMesh mesh = new TriMesh(vBuffer, iBuffer, false);
//		System.err.println( "Scale mesh = full " + testIntersections(mesh) );
//		growMesh( mesh, wormDiameters, grow );
		
		return mesh;
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
    
    
    public void moveSelectedPoint( Vector3f direction )
    {
    	if ( pickedPoint != null )
    	{
    		pickedPoint.add(direction);
        	updateLattice(false);
        	return;
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
			updateLattice(true);
        }

    }
	
	public void saveLatticeStatistics( ModelImage image, float length, VOIContour left, VOIContour right, 
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
        	bw.write( "Total Length:," +  JDialogLattice.VoxelSize * length + "\n" );
            bw.newLine();
        	bw.write( "pair" + "," + "diameter" + ","  + "left distance" + "," + "right distance" + "\n" );
        	for ( int i = 0; i < leftPairs.length; i++ )
        	{
        		bw.write(i + "," + JDialogLattice.VoxelSize * left.elementAt(i).distance(right.elementAt(i)) + "," + JDialogLattice.VoxelSize * leftPairs[i] + "," + JDialogLattice.VoxelSize * rightPairs[i] + "\n");
        	}
            bw.newLine();
        	bw.close();
        } catch (final Exception e) {
        	System.err.println("CAUGHT EXCEPTION WITHIN writeXML() of FileVOI");
        	e.printStackTrace();
        }
    }
	
    public void showModel( )
	{
		if ( (imageA.isRegistered( displayContours ) == -1) )
		{
			imageA.registerVOI(displayContours);
	        imageA.notifyImageDisplayListeners();
		}
		else if ( (imageA.isRegistered( displayContours ) != -1) )
		{
			imageA.unregisterVOI(displayContours);
	        imageA.notifyImageDisplayListeners();
		}
		
	}
    
    public NaturalSpline3 smoothCurve( VOIContour curve, float[] time )
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
    
    


	public NaturalSpline3 smoothCurve2( VOIContour curve, float[] time )
    {
    	Vector3f[] akPoints = new Vector3f[curve.size()];
    	for ( int i = 0; i < curve.size(); i++ )
    	{
    		akPoints[i] = new Vector3f(curve.elementAt(i));
    	}
    	
    	return new NaturalSpline3( NaturalSpline3.BoundaryType.BT_FREE, curve.size()-1, time, akPoints );
    }
	
    public ModelImage straighten( ModelImage image, VOI samplingPlanes, Vector<Ellipsoid3f> ellipseBounds,
    		int diameter, int[] latticeSlice, boolean saveStats )
    {
    	
		int colorFactor = image.isColorImage() ? 4 : 1;
		int[] resultExtents = new int[]{diameter, diameter, samplingPlanes.getCurves().size()};
		float[][] values = new float[resultExtents[2]][resultExtents[0] * resultExtents[1] * colorFactor]; 
		float[][] dataOrigin = new float[resultExtents[2]][resultExtents[0] * resultExtents[1] * 4]; 

    	String imageName = image.getImageName();
    	if ( imageName.contains("_clone") )
    	{
    		imageName = imageName.replaceAll("_clone", "" );
    	}
		ModelImage resultImage = new ModelImage(image.getType(), resultExtents, imageName + "_straight.xml");
		JDialogBase.updateFileInfo( image, resultImage );
		resultImage.setResolutions( new float[]{1,1,1});
		
		ModelImage straightToOrigin = new ModelImage( ModelStorageBase.ARGB_FLOAT, resultExtents, imageName + "_toOriginal.xml");
		JDialogBase.updateFileInfo( image, straightToOrigin );
		straightToOrigin.setResolutions( new float[]{1,1,1});
		for ( int i = 0; i < straightToOrigin.getDataSize(); i++ )
		{
			straightToOrigin.set(i, 0);
		}
		
		Vector3f lpsOrigin = new Vector3f();
		for( int i = 0; i < samplingPlanes.getCurves().size(); i++ )
		{
			VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
	        Vector3f[] corners = new Vector3f[4];
	        for ( int j = 0; j < 4; j++ )
	        {
	        	corners[j] = kBox.elementAt(j);
	        }
			try {
				image.exportDiagonal( 0, i, resultExtents, corners, 
						ellipseBounds.elementAt(i), values[i], true, dataOrigin[i]);

				if ( i == 0 )
				{
					MipavCoordinateSystems.fileToScanner( corners[0], lpsOrigin, image );
				}

				resultImage.importData(i*values[i].length, values[i], false);
				straightToOrigin.importData(i*dataOrigin[i].length, dataOrigin[i], false);
			} catch(IOException e) {
				e.printStackTrace();
			}
		}
		

		float[] leftDistances = new float[latticeSlice.length];
		float[] rightDistances = new float[latticeSlice.length];
		short id = (short) image.getVOIs().getUniqueID();
		VOI lattice = new VOI(id, "lattice", VOI.POLYLINE, (float)Math.random() );
		VOIContour leftSide = new VOIContour( false );
		VOIContour rightSide = new VOIContour( false );
		lattice.getCurves().add(leftSide);		
		lattice.getCurves().add(rightSide);
		Vector3f dir = new Vector3f(1,0,0);
		for ( int i = 0; i < latticeSlice.length; i++ )
		{
			Ellipsoid3f ellipsoid = ellipseBounds.elementAt( latticeSlice[i] );
//			Vector3f center = ellipsoid.Center;
			float width = ellipsoid.Extent[0] - DiameterBuffer;
//			Vector3f dir = ellipsoid.Axis[0];
			Vector3f center = new Vector3f(diameter/2,diameter/2,latticeSlice[i]);
			
			Vector3f leftPt = Vector3f.scale( -width, dir ); leftPt.add(center);
			leftSide.add(leftPt);
			
			Vector3f rightPt = Vector3f.scale(  width, dir ); rightPt.add(center);
			rightSide.add(rightPt);

			leftDistances[i] = 0;
			rightDistances[i] = 0;
			if ( i > 0 )
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
		}
		resultImage.calcMinMax();
		new ViewJFrameImage(resultImage);  	
		saveTransformImage(imageName, resultImage);

		if ( saveStats )
		{
			saveLatticeStatistics(image, resultExtents[2], leftSide, rightSide, leftDistances, rightDistances, "_after");			
			saveTransformImage(imageName, straightToOrigin);
			ModelImage originToStraight = computeOriginToStraight(image, straightToOrigin);
			saveTransformImage(imageName, originToStraight);
			
			ModelImage croppedVolume = computeMissingData(originToStraight);
			saveTransformImage(imageName, croppedVolume);

//			testTransform( resultImage, straightToOrigin, image.getExtents() );
//			testTransform( image, originToStraight, resultImage.getExtents() );
		}
		
		
		
		return resultImage;
    }
    

    private ModelImage computeOriginToStraight( ModelImage originalImage, ModelImage straightToOrigin )
    {
    	String imageName = originalImage.getImageName();
    	if ( imageName.contains("_clone") )
    	{
    		imageName = imageName.replaceAll("_clone", "" );
    	}
    	ModelImage originToStraight = new ModelImage( ModelStorageBase.ARGB_FLOAT, originalImage.getExtents(),  imageName + "_toStraight.xml");
		JDialogBase.updateFileInfo( originalImage, originToStraight );
		for ( int i = 0; i < originToStraight.getDataSize(); i++ )
		{
			originToStraight.set(i, 0);
		}
		
    	int dimX = straightToOrigin.getExtents().length > 0 ? straightToOrigin.getExtents()[0] : 1;
    	int dimY = straightToOrigin.getExtents().length > 1 ? straightToOrigin.getExtents()[1] : 1;
    	int dimZ = straightToOrigin.getExtents().length > 2 ? straightToOrigin.getExtents()[2] : 1;
    	
    	int[] outputExtents = originalImage.getExtents();
    	
    	for ( int z = 0; z < dimZ; z++ )
    	{
    		for ( int y = 0; y < dimY; y++ )
    		{
    			for ( int x = 0; x < dimX; x++ )
    			{
    				float a = straightToOrigin.getFloatC(x, y, z, 0);
    				if ( a == 1 )
    				{
    					int inputIndex = z * dimX * dimY + y * dimX + x;
    					int outputX = Math.round(straightToOrigin.getFloat( (inputIndex * 4) + 1));
    					int outputY = Math.round(straightToOrigin.getFloat( (inputIndex * 4) + 2));
    					int outputZ = Math.round(straightToOrigin.getFloat( (inputIndex * 4) + 3));

    					int outputIndex = outputZ * outputExtents[0] * outputExtents[1] + outputY * outputExtents[0] + outputX;
    					originToStraight.set( (outputIndex * 4) + 0, 1);
    					originToStraight.set( (outputIndex * 4) + 1, x);
    					originToStraight.set( (outputIndex * 4) + 2, y);
    					originToStraight.set( (outputIndex * 4) + 3, z);
    				}
    			}
    		}
    	}
    	return originToStraight;
    }
    
    private ModelImage computeMissingData( ModelImage originToStraight )
    {
    	
    	ModelImage croppedMask = new ModelImage( ModelStorageBase.BOOLEAN, originToStraight.getExtents(),  "cropped_mask.xml");
		JDialogBase.updateFileInfo( originToStraight, croppedMask );	
		
    	int dimX = croppedMask.getExtents().length > 0 ? croppedMask.getExtents()[0] : 1;
    	int dimY = croppedMask.getExtents().length > 1 ? croppedMask.getExtents()[1] : 1;
    	int dimZ = croppedMask.getExtents().length > 2 ? croppedMask.getExtents()[2] : 1;

    	int count = 0;
    	for ( int z = 0; z < dimZ; z++ )
    	{
    		for ( int y = 0; y < dimY; y++ )
    		{
    			for ( int x = 0; x < dimX; x++ )
    			{
    				int index = z*dimY*dimX + y*dimX + x;
    				croppedMask.set(index, false);
    				if ( originToStraight.getFloat(index * 4) > 0 )
    				{
    					croppedMask.set(index,true);
    					count++;
    				}
    			}
    		}
    	}
    	int size = dimX*dimY*dimZ;
//    	System.err.println( "Percent used = " + (100f*((float)count/(float)size)) );
		
    	return croppedMask;
    }
    
    
    


    public void exportDiagonal( ModelImage image, ModelImage model, ModelImage insideConflict,
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
        center.scale( 1f/(float)verts.length );
        
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

        xSlopeX /= (iBound - 1);
        ySlopeX /= (iBound - 1);
        zSlopeX /= (iBound - 1);

        xSlopeY /= (jBound - 1);
        ySlopeY /= (jBound - 1);
        zSlopeY /= (jBound - 1);

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
                final int iIndex = (int) Math.round(x);
                final int jIndex = (int) Math.round(y);
                final int kIndex = (int) Math.round(z);

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
    										model.set(x1, y1, z1, (currentValue + value)/2f);
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


    public VOIContour growDiagonalX( ModelImage image, ModelImage model, final int tSlice, final int slice, final int[] extents,
            final Vector3f[] verts, final float value) 
    {
        final int iBound = extents[0];
        final int iBoundHalf = (int) (extents[0]/2f);
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
        center.scale( 1f/(float)verts.length );
        
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

        xSlopeX /= (iBound - 1);
        ySlopeX /= (iBound - 1);
        zSlopeX /= (iBound - 1);

        xSlopeY /= (jBound - 1);
        ySlopeY /= (jBound - 1);
        zSlopeY /= (jBound - 1);
        
               
        
        VOIContour edgePts = new VOIContour(true);
        Vector3f currentPoint = new Vector3f();

        /* loop over the 2D image (values) we're writing into */
        for (int j = jBound - 1; j >= 0; j--) {
            // half-way along the x-axis:
            x0 = verts[0].X + j*xSlopeY;
            y0 = verts[0].Y + j*ySlopeY;
            z0 = verts[0].Z + j*zSlopeY;

            float prevValue = -1;
            for (int i = iBoundHalf; i >= 0; i--)
            {
                float x = x0 + i*xSlopeX;
                float y = y0 + i*ySlopeX;
                float z = z0 + i*zSlopeX;
                
                final int iIndex = (int) Math.round(x);
                final int jIndex = (int) Math.round(y);
                final int kIndex = (int) Math.round(z);

                /* calculate the ModelImage space index: */
                final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));

                // Bounds checking:
                if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
                		|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > image.getSize()))) {

                	// do nothing
                } 
                else
                {
                	float currentValue = model.getFloat(index);
        			currentPoint.set(x, y, z);
                	if ( currentValue == 0 )
                	{
                		if ( (prevValue > 0) && (Math.abs(prevValue - value) < SampleLimit) )
                		{
//                			if ( ContBox3f.InBox( currentPoint, boxBound ) && (center.distance(currentPoint) < diameter) )
//                			{
//                			}
                            Vector3f edge = new Vector3f(currentPoint);
                            if ( !edgePts.contains(edge) )
                            {
                            	edgePts.add( edge );
                            }
                		}
                		break;
                	}
                	if ( Math.abs(currentValue - value) > SampleLimit )
                	{
//                        Vector3f edge = new Vector3f(currentPoint);
//                        if ( !edgePts.contains(edge) )
//                        {
//                        	edgePts.add( edge );
//                        }
                		break;
                	}
                	prevValue = currentValue;
                }
            }         
        }
        
        for (int j = 0; j < jBound; j++) {

            // half-way along the x-axis:
            x0 = verts[0].X + j*xSlopeY;
            y0 = verts[0].Y + j*ySlopeY;
            z0 = verts[0].Z + j*zSlopeY;

            float prevValue = -1;
            for (int i = iBoundHalf+1; i < iBound; i++)
            {
                float x = x0 + i*xSlopeX;
                float y = y0 + i*ySlopeX;
                float z = z0 + i*zSlopeX;
                
                final int iIndex = (int) Math.round(x);
                final int jIndex = (int) Math.round(y);
                final int kIndex = (int) Math.round(z);

                /* calculate the ModelImage space index: */
                final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));

                // Bounds checking:
                if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
                		|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > image.getSize()))) {

                	// do nothing
                } 
                else
                {
                	float currentValue = model.getFloat(index);
        			currentPoint.set(x, y, z);
                	if ( currentValue == 0 )
                	{
                		if ( (prevValue > 0) && (Math.abs(prevValue - value) < SampleLimit) )
                		{
//                			currentPoint.set(x, y, z);
//                			if ( ContBox3f.InBox( currentPoint, boxBound ) && (center.distance(currentPoint) < diameter) )
//                			{
//                                Vector3f edge = new Vector3f(x0 + endIndex*xSlopeX, y0 + endIndex*ySlopeX, z0 + endIndex*zSlopeX);
//                                if ( !edgePts.contains(edge) )
//                                {
//                                	edgePts.add( edge );
//                                }  
//                			}
                            Vector3f edge = new Vector3f(currentPoint);
                            if ( !edgePts.contains(edge) )
                            {
                            	edgePts.add( edge );
                            }  
                		}
                		break;
                	}
                	if ( Math.abs(currentValue - value) > SampleLimit )
                	{
//                        Vector3f edge = new Vector3f(currentPoint);
//                        if ( !edgePts.contains(edge) )
//                        {
//                        	edgePts.add( edge );
//                        }  
                		break;
                	}
                	prevValue = currentValue;
                }
            }
        }           
        return edgePts;
    }
    



    public VOIContour growDiagonalY( ModelImage image, ModelImage model, final int tSlice, final int slice, final int[] extents,
            final Vector3f[] verts, final float value) 
    {
        final int iBound = extents[0];
        final int jBound = extents[1];
        final int jBoundHalf = (int) (extents[1]/2f);

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
        center.scale( 1f/(float)verts.length );
        
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

        xSlopeX /= (iBound - 1);
        ySlopeX /= (iBound - 1);
        zSlopeX /= (iBound - 1);

        xSlopeY /= (jBound - 1);
        ySlopeY /= (jBound - 1);
        zSlopeY /= (jBound - 1);
        
               
        
        VOIContour edgePts = new VOIContour(true);
        Vector3f currentPoint = new Vector3f();

        /* loop over the 2D image (values) we're writing into */
        for (int i = iBound - 1; i >= 0; i--) {
            x0 = verts[0].X + i*xSlopeX;
            y0 = verts[0].Y + i*ySlopeX;
            z0 = verts[0].Z + i*zSlopeX;

            float prevValue = -1;
            for (int j = jBoundHalf; j >= 0; j--)
            {
                float x = x0 + j*xSlopeY;
                float y = y0 + j*ySlopeY;
                float z = z0 + j*zSlopeY;
                
                final int iIndex = (int) Math.round(x);
                final int jIndex = (int) Math.round(y);
                final int kIndex = (int) Math.round(z);

                /* calculate the ModelImage space index: */
                final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));

                // Bounds checking:
                if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
                		|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > image.getSize()))) {

                	// do nothing
                } 
                else
                {
                	float currentValue = model.getFloat(index);
        			currentPoint.set(x, y, z);
                	if ( currentValue == 0 )
                	{
                		if ( (prevValue > 0) && (Math.abs(prevValue - value) < SampleLimit) )
                		{
//                			if ( ContBox3f.InBox( currentPoint, boxBound ) && (center.distance(currentPoint) < diameter) )
//                			{
//                			}
                            Vector3f edge = new Vector3f(currentPoint);
                            if ( !edgePts.contains(edge) )
                            {
                            	edgePts.add( edge );
                            }
                		}
                		break;
                	}
                	if ( Math.abs(currentValue - value) > SampleLimit )
                	{
//                        Vector3f edge = new Vector3f(currentPoint);
//                        if ( !edgePts.contains(edge) )
//                        {
//                        	edgePts.add( edge );
//                        }
                		break;
                	}
                	prevValue = currentValue;
                }
            }         
        }
        
        
        /* loop over the 2D image (values) we're writing into */
        for (int i = 0; i < iBound; i++) {
            x0 = verts[0].X + i*xSlopeX;
            y0 = verts[0].Y + i*ySlopeX;
            z0 = verts[0].Z + i*zSlopeX;

            float prevValue = -1;
            for (int j = jBoundHalf+1; j < jBound; j++)
            {
                float x = x0 + j*xSlopeY;
                float y = y0 + j*ySlopeY;
                float z = z0 + j*zSlopeY;
                
                final int iIndex = (int) Math.round(x);
                final int jIndex = (int) Math.round(y);
                final int kIndex = (int) Math.round(z);

                /* calculate the ModelImage space index: */
                final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));

                // Bounds checking:
                if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
                		|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > image.getSize()))) {

                	// do nothing
                } 
                else
                {
                	float currentValue = model.getFloat(index);
        			currentPoint.set(x, y, z);
                	if ( currentValue == 0 )
                	{
                		if ( (prevValue > 0) && (Math.abs(prevValue - value) < SampleLimit) )
                		{
//                			if ( ContBox3f.InBox( currentPoint, boxBound ) && (center.distance(currentPoint) < diameter) )
//                			{
//                			}
                            Vector3f edge = new Vector3f(currentPoint);
                            if ( !edgePts.contains(edge) )
                            {
                            	edgePts.add( edge );
                            }
                		}
                		break;
                	}
                	if ( Math.abs(currentValue - value) > SampleLimit )
                	{
//                        Vector3f edge = new Vector3f(currentPoint);
//                        if ( !edgePts.contains(edge) )
//                        {
//                        	edgePts.add( edge );
//                        }
                		break;
                	}
                	prevValue = currentValue;
                }
            }         
        }
        return edgePts;
    }
    


    private void growEdges( ModelImage model, Vector<VOIContour> edgeLists )
    {
    	int dimX = model.getExtents().length > 0 ? model.getExtents()[0] : 1;
    	int dimY = model.getExtents().length > 1 ? model.getExtents()[1] : 1;
    	int dimZ = model.getExtents().length > 2 ? model.getExtents()[2] : 1;    	
    	
    	for ( int i = 0; i < edgeLists.size(); i++ )
    	{
			int value = i+1;
			VOIContour contour = edgeLists.elementAt(i);
    		for ( int j = 0; j < contour.size(); j++ )
    		{
    			Vector3f pt = contour.elementAt(j);
    			float x = pt.X;
    			float y = pt.Y;
    			float z = pt.Z;
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
                            	if ( currentValue != 0 )
                            	{
									if ( Math.abs(currentValue - value) < SampleLimit )
									{
										model.set(x1, y1, z1, (currentValue + value)/2f);
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
    	}
    	
    }
    
    
    public void writeDiagonal( ModelImage image, ModelImage model, final int tSlice, final int slice, final int[] extents,
            final Vector3f[] verts, final float[] values, float[] dataOrigin) 
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
        center.scale( 1f/(float)verts.length );
        
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

        xSlopeX /= (iBound - 1);
        ySlopeX /= (iBound - 1);
        zSlopeX /= (iBound - 1);

        xSlopeY /= (jBound - 1);
        ySlopeY /= (jBound - 1);
        zSlopeY /= (jBound - 1);

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
                final int iIndex = (int) Math.round(x);
                final int jIndex = (int) Math.round(y);
                final int kIndex = (int) Math.round(z);

                /* calculate the ModelImage space index: */
                final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));

                // Bounds checking:
                if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
                		|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > image.getSize()))) {

                	// do nothing
                } else {
                	float currentValue = (slice+1);
                	if ( model != null )
                	{
                		currentValue = model.getFloat((int)x, (int)y, (int)z);
                	}
                	if ( currentValue == 0 )
                	{
                    	if ( buffFactor == 4 ) {
                    		values[ ( ( (j * iBound) + i) * 4) + 0] = (float)image.getMin();
                    		values[ ( ( (j * iBound) + i) * 4) + 1] = (float)image.getMin();
                    		values[ ( ( (j * iBound) + i) * 4) + 2] = (float)image.getMin();
                    		values[ ( ( (j * iBound) + i) * 4) + 3] = (float)image.getMin();
                    	}
                    	/* not color: */
                    	else {
                    		values[ (j * iBound) + i] = (float)image.getMin();
                    	}                		
                	}
                	else if ( Math.abs(currentValue - (slice+1)) < SampleLimit )
                	{
                    	/* if color: */
                    	if ( buffFactor == 4 ) {
                    		values[ ( ( (j * iBound) + i) * 4) + 0] = image.getFloat( (index * 4) + 0);
                    		values[ ( ( (j * iBound) + i) * 4) + 1] = image.getFloat( (index * 4) + 1);
                    		values[ ( ( (j * iBound) + i) * 4) + 2] = image.getFloat( (index * 4) + 2);
                    		values[ ( ( (j * iBound) + i) * 4) + 3] = image.getFloat( (index * 4) + 3);
                    	}
                    	/* not color: */
                    	else {
                    		values[ (j * iBound) + i] = image.getFloatTriLinearBounds(x, y, z);
                    	}
                    	dataOrigin[ ( ( (j * iBound) + i) * 4) + 0] = 1;
                    	dataOrigin[ ( ( (j * iBound) + i) * 4) + 1] = x;
                    	dataOrigin[ ( ( (j * iBound) + i) * 4) + 2] = y;
                    	dataOrigin[ ( ( (j * iBound) + i) * 4) + 3] = z;
                		
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
        
//        checkConvex( values, dataOrigin, (float)image.getMin(), outsideVal, buffFactor, iBound, jBound, iBoundHalf, jBoundHalf );
    }
    
    
    
    
    

    
    

    public void generateMasks( ModelImage imageA, ModelImage imageB, VOI samplingPlanes, 
    		Vector<Ellipsoid3f> ellipseBounds, Vector<Float> diameters, int diameter, boolean straighten, boolean displayResult  )
    {
		int[] resultExtents = new int[]{diameter, diameter, samplingPlanes.getCurves().size()};
		
    	String imageName = imageA.getImageName();
    	if ( imageName.contains("_clone") )
    	{
    		imageName = imageName.replaceAll("_clone", "" );
    	}
		ModelImage model = new ModelImage(imageA.getType(), imageA.getExtents(), imageName + "_model.xml");
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
    	
    	float maxDiameter = -1;
		for ( int i = 0; i < samplingPlanes.getCurves().size(); i++ )
		{
			VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
			Vector3f[] corners = new Vector3f[4];
			for ( int j = 0; j < 4; j++ )
			{
				corners[j] = kBox.elementAt(j);
			}
			exportDiagonal( imageA, model, insideConflict, 0, i, resultExtents, corners, 
					ellipseBounds.elementAt(i), 1.5f*diameters.elementAt(i), boxBounds.elementAt(i), i+1);
			if ( 1.5f*diameters.elementAt(i) > maxDiameter )
			{
				maxDiameter = 1.5f*diameters.elementAt(i);
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
    	insideConflict.disposeLocal();
    	insideConflict = null;

    	short sID = (short)(imageA.getVOIs().getUniqueID());
    	displayInterpolatedContours = new VOI(sID, "interpolatedContours");
    	displayInterpolatedContours.setColor( Color.blue );
		for ( int d = 0; d < 10; d++ )
		{		
			Vector<VOIContour> edgesX = new Vector<VOIContour>();
			Vector<VOIContour> edgesY = new Vector<VOIContour>();
			for ( int i = 0; i < samplingPlanes.getCurves().size(); i++ )
			{
				VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
				Vector3f[] corners = new Vector3f[4];
				for ( int j = 0; j < 4; j++ )
				{
					corners[j] = kBox.elementAt(j);
				}
				edgesX.add( growDiagonalX( imageA, model, 0, i, resultExtents, corners, i+1) );
				edgesY.add( growDiagonalY( imageA, model, 0, i, resultExtents, corners, i+1) );
				if ( !straighten )
				{
					if ( d == 9 )
					{
						if ( (i%30) == 0 )
						{
							VOIContour contour = edgesX.elementAt(i);
							displayInterpolatedContours.getCurves().add(contour);
							contour.update( new ColorRGBA(0,0,1,1));
							contour.setVolumeDisplayRange(minRange);

							contour = edgesY.elementAt(i);
							displayInterpolatedContours.getCurves().add(contour);
							contour.update( new ColorRGBA(0,0,1,1));
							contour.setVolumeDisplayRange(minRange);
						}
					}
				}
			}
			growEdges( model, edgesX );
			growEdges( model, edgesY );
		}
		
//		model.calcMinMax();
//		new ViewJFrameImage((ModelImage)model.clone());
		if ( !straighten )
		{
			imageA.registerVOI(displayInterpolatedContours);			
		}
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
			saveTransformImage(imageName, inside);

			straighten(imageA, resultExtents, imageName, model, true, displayResult );
			//		straighten(model, resultExtents, imageName, null, false );
			straighten(inside, resultExtents, imageName, model, false, displayResult );
			if ( imageB != null )
			{
				straighten(imageB, resultExtents, imageName, model, false, displayResult );			
			}
		}
		inside.disposeLocal();
		inside = null;
		model.disposeLocal();
		model = null;
    }
    
    private ModelImage straighten( ModelImage image, int[] resultExtents, String baseName, ModelImage model, boolean saveStats, boolean displayResult )
    {
    	String imageName = image.getImageName();
    	if ( imageName.contains("_clone") )
    	{
    		imageName = imageName.replaceAll("_clone", "" );
    	}

		int colorFactor = image.isColorImage() ? 4 : 1;
		float[][] values = new float[resultExtents[2]][resultExtents[0] * resultExtents[1] * colorFactor]; 
		float[][] dataOrigin = new float[resultExtents[2]][resultExtents[0] * resultExtents[1] * 4]; 

		ModelImage resultImage = new ModelImage(image.getType(), resultExtents, imageName + "_straight.xml");
		JDialogBase.updateFileInfo( image, resultImage );
		resultImage.setResolutions( new float[]{1,1,1});
		
		ModelImage straightToOrigin = new ModelImage( ModelStorageBase.ARGB_FLOAT, resultExtents, imageName + "_toOriginal.xml");
		JDialogBase.updateFileInfo( image, straightToOrigin );
		straightToOrigin.setResolutions( new float[]{1,1,1});
		for ( int i = 0; i < straightToOrigin.getDataSize(); i++ )
		{
			straightToOrigin.set(i, 0);
		}

		for( int i = 0; i < samplingPlanes.getCurves().size(); i++ )
		{
//			float diameterInterp = samplingDiameters.elementAt(i);
			VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
	        Vector3f[] corners = new Vector3f[4];
	        for ( int j = 0; j < 4; j++ )
	        {
	        	corners[j] = kBox.elementAt(j);
	        }
			try {
				writeDiagonal( image, model, 0, i, resultExtents, corners, values[i], dataOrigin[i]);
				resultImage.importData(i*values[i].length, values[i], false);
				straightToOrigin.importData(i*dataOrigin[i].length, dataOrigin[i], false);
			} catch(IOException e) {
				e.printStackTrace();
			}
		}
		
		

		

		float[] leftDistances = new float[latticeSlice.length];
		float[] rightDistances = new float[latticeSlice.length];
		short id = (short) image.getVOIs().getUniqueID();
		VOI lattice = new VOI(id, "lattice", VOI.POLYLINE, (float)Math.random() );
		VOIContour leftSide = new VOIContour( false );
		VOIContour rightSide = new VOIContour( false );
		lattice.getCurves().add(leftSide);		
		lattice.getCurves().add(rightSide);
		Vector3f dir = new Vector3f(1,0,0);
		for ( int i = 0; i < latticeSlice.length; i++ )
		{
			Ellipsoid3f ellipsoid = ellipseBounds.elementAt( latticeSlice[i] );
//			Vector3f center = ellipsoid.Center;
			float width = ellipsoid.Extent[0] - DiameterBuffer;
//			Vector3f dir = ellipsoid.Axis[0];
			Vector3f center = new Vector3f(resultExtents[0]/2,resultExtents[0]/2,latticeSlice[i]);
			
			Vector3f leftPt = Vector3f.scale( -width, dir ); leftPt.add(center);
			leftSide.add(leftPt);
			
			Vector3f rightPt = Vector3f.scale(  width, dir ); rightPt.add(center);
			rightSide.add(rightPt);

			leftDistances[i] = 0;
			rightDistances[i] = 0;
			if ( i > 0 )
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
		}
		
		resultImage.calcMinMax();
		if ( displayResult )
		{
			new ViewJFrameImage(resultImage);
		}

		saveTransformImage(baseName, resultImage);
		if ( saveStats )
		{
			saveLatticeStatistics(image, resultExtents[2], leftSide, rightSide, leftDistances, rightDistances, "_after");
			saveTransformImage(baseName, straightToOrigin);
			ModelImage originToStraight = computeOriginToStraight(image, straightToOrigin);
			saveTransformImage(baseName, originToStraight);
			originToStraight.disposeLocal();
			originToStraight = null;
			//		testTransform( resultImage, straightToOrigin, image.getExtents() );
			//		testTransform( image, originToStraight, resultImage.getExtents() );
		}
		straightToOrigin.disposeLocal();
		straightToOrigin = null;
		
		return resultImage;
    }
    
    
    private void generateCurves( )
    {
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

		centerTangents = new Vector<Vector3f>();
	    wormDiameters = new Vector<Float>();
	    rightVectors = new Vector<Vector3f>();
	    upVectors = new Vector<Vector3f>();
	    
		
		length = centerSpline.GetLength(0, 1);
		allTimes = new float[(int) (Math.ceil(length))];
		
		for ( int i = 0; i < length; i++ )
		{
			float t = centerSpline.GetTime(i);
			centerPositions.add(centerSpline.GetPosition(t));
			leftPositions.add(leftSpline.GetPosition(t));
			rightPositions.add(rightSpline.GetPosition(t));
			
			
			allTimes[i] = t;
			centerTangents.add( centerSpline.GetFirstDerivative(t) );
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
			
			centerTangents.elementAt(i).normalize();
			Vector3f upDir = Vector3f.cross( rightDir, centerTangents.elementAt(i) );
			upDir.normalize();
			upVectors.add(upDir);
			float curve = centerSpline.GetSecondDerivative(t).length();
        	if ( curve < minCurve )
        	{
        		minCurve = curve;
        	}
        	if ( curve > maxCurve )
        	{
        		maxCurve = curve;
        	}
		}
		extent += 10;
		
//		boxBounds = new Vector<Box3f>();
//		ellipseBounds = new Vector<Ellipsoid3f>();
//		ellipseOuterBounds = new Vector<Ellipsoid3f>();
		sID = (short)(imageA.getVOIs().getUniqueID());
//		samplingPlanes = new VOI(sID, "samplingPlanes");
		displayContours = new VOI(sID, "wormContours");
		for ( int i = 0; i < centerPositions.size(); i += 30 )
		{
	        Vector3f rkEye = centerPositions.elementAt(i);
	        Vector3f rkRVector = rightVectors.elementAt(i);
	        Vector3f rkUVector = upVectors.elementAt(i);
	        
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
//			VOIContour kBox = new VOIContour(true);
//			for ( int j = 0; j < 4; j++ )
//			{
//				kBox.addElement( output[j].X, output[j].Y, output[j].Z );
//			}
//			kBox.update( new ColorRGBA(0,0,1,1) );		
//	        {	
//	        	samplingPlanes.importCurve(kBox);
//	        }
//	        
//
	        float curve = centerSpline.GetSecondDerivative(allTimes[i]).length();
	        float scale = (curve - minCurve)/(maxCurve - minCurve);
	        VOIContour ellipse = new VOIContour(true);
	        ellipse.setVolumeDisplayRange(minRange);
	        makeEllipse2( rkRVector, rkUVector, rkEye, wormDiameters.elementAt(i), scale, ellipse );
	        displayContours.getCurves().add(ellipse);
//	        
//
//	        ellipse = new VOIContour(true);
//	        ellipsoid = makeEllipse( rkRVector, rkUVector, rkEye, wormDiameters.elementAt(i), 0, ellipse );
//	        ellipseOuterBounds.add( ellipsoid );
//	        
//	        
//	        Box3f box = new Box3f( ellipsoid.Center, ellipsoid.Axis, new float[]{extent, extent, 1 } );
//	        boxBounds.add(box);	        
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
    
    private void makeEllipse2( Vector3f right, Vector3f up, Vector3f center, float diameterA, float scale, VOIContour ellipse  )
	{
		int numPts = 32;
		float diameterB = diameterA/2f + (1-scale) * diameterA/2f;
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
    
    private void saveMesh( ModelImage image, TriMesh mesh, final boolean flip ) 
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
    			"worm_model" + File.separator;
        voiFileDir = new File(voiDir);
        if (voiFileDir.exists() && voiFileDir.isDirectory()) {
//        	String[] list = voiFileDir.list();
//        	for ( int i = 0; i < list.length; i++ )
//        	{
////        		System.err.println( list[i] );
//        		File meshFile = new File( voiDir + list[i] );
//        		meshFile.delete();
//        	}
        } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {
        } else { // voiFileDir does not exist
            voiFileDir.mkdir();
        }
        
        String kName = voiDir + "worm_surface.xml";
        
        TriMesh kMesh = new TriMesh( new VertexBuffer(transformedPositions), new IndexBuffer(mesh.IBuffer), false);
        try {
			FileSurface_WM.save(kName, kMesh, 0, kMesh.VBuffer, flip, direction, startLocation, box, inverseDicomMatrix);
		} catch (IOException e) {}
    }    
    
    private void saveTransformImage( String imageName, ModelImage image  ) 
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
        ModelImage.saveImage( image, image.getImageName() + ".xml", voiDir );
    }
    
    
    private boolean testIntersections( TriMesh mesh  )
	{
		int numTriangles = mesh.GetTriangleQuantity();
		int[] indices = mesh.IBuffer.GetData();

	    for (int i = 0; i < numTriangles; ++i)
	    {
	        int v0 = indices[i*3 + 0];
	        int v1 = indices[i*3 + 1];
	        int v2 = indices[i*3 + 2];
	        Vector3f p0 = mesh.VBuffer.GetPosition3( v0 );
	        Vector3f p1 = mesh.VBuffer.GetPosition3( v1 );
	        Vector3f p2 = mesh.VBuffer.GetPosition3( v2 );
	        Triangle3f t0 = new Triangle3f(p0, p1, p2);
	        
		    for (int j = i+1; j < numTriangles; ++j)
		    {
		        int v10 = indices[j*3 + 0];
		        int v11 = indices[j*3 + 1];
		        int v12 = indices[j*3 + 2];
		        if ( v0 == v10 || v0 == v11 || v0 == v12 ||
			         v1 == v10 || v1 == v11 || v1 == v12 ||
			         v2 == v10 || v2 == v11 || v2 == v12 )
		        {
		        	continue;
		        }
		        
		        
		        Vector3f p10 = mesh.VBuffer.GetPosition3( v10 );
		        Vector3f p11 = mesh.VBuffer.GetPosition3( v11 );
		        Vector3f p12 = mesh.VBuffer.GetPosition3( v12 );
		        
		        if ( p0.isEqual(p10) || p0.isEqual(p11) || p0.isEqual(p12) ||
			         p1.isEqual(p10) || p1.isEqual(p11) || p1.isEqual(p12) ||
			         p2.isEqual(p10) || p2.isEqual(p11) || p2.isEqual(p12) )
		        {
		        	continue;
		        }
		        
		        
		        Triangle3f t1 = new Triangle3f(p10, p11, p12);
		        
		        IntrTriangle3Triangle3f intersector = new IntrTriangle3Triangle3f( t0, t1 );
		        if ( intersector.Test() )
		        {			        
			        if ( intersector.Find() )
			        {
			        	return true;
			        }
		        }
		    }
	    }

	    return false;
	}
    
    
    private void testTransform( ModelImage original, ModelImage transform, int[] outputExtents )
    {
    	ModelImage output = new ModelImage( original.getType(), outputExtents, original.getImageName() + "_test_transformed" );

		
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
    	original.calcMinMax();
    	transform.calcMinMax();
    	new ViewJFrameImage(output);
    	new ViewJFrameImage(original);
    	new ViewJFrameImage(transform);
    }
    
    private void updateLattice( boolean rebuild )
	{
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
			for ( int j = 0; j < left.size(); j++ )
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
        	
        generateCurves();
		if ( showContours )
		{
			imageA.registerVOI( displayContours );
		}
        
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
        
        // when everything's done, notify the image listeners
        imageA.notifyImageDisplayListeners();
	}
    
}
