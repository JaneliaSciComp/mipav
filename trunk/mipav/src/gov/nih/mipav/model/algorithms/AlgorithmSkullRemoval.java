package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Distance.DistanceVector3Plane3;
import WildMagic.LibFoundation.Intersection.IntrLine3Triangle3f;
import WildMagic.LibFoundation.Mathematics.Line3f;
import WildMagic.LibFoundation.Mathematics.Plane3f;
import WildMagic.LibFoundation.Mathematics.Triangle3f;
import WildMagic.LibFoundation.Mathematics.Vector2d;
import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Meshes.ConvexHull3f;
import WildMagic.LibFoundation.Meshes.VETMesh;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.BoxBV;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.Transformation;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

import gov.nih.mipav.model.algorithms.filters.AlgorithmGaussianBlurSep;
import gov.nih.mipav.model.algorithms.filters.OpenCL.filters.OpenCLAlgorithmGaussianBlur;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageCalculator;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageMath;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmMask;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;

import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogFaceAnonymize;
import gov.nih.mipav.view.renderer.WildMagic.Interface.FileSurface_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.SurfaceExtractorCubes;
import gov.nih.mipav.view.renderer.WildMagic.brainflattenerview_WM.MjCorticalMesh_WM;

import java.awt.Color;
import java.io.IOException;
import java.util.*;


public class AlgorithmSkullRemoval extends AlgorithmBase
{    
    private int faceOrientation;
	private float min_2P = -1;
	private float max_98P = -1;
	private boolean useCSFMin = true;
	private float csfThresholdMin = -1;
	private float csfThresholdMax = -1;
	private float wmThreshold = -1;
	private float gmThresholdMin = -1;
	private float gmThresholdMax = -1;
	private float maxRadius = -1;
	private Vector3f cog;
	private Vector3f cogMax;
	private int dimX;
	private int dimY;
	private int dimZ;

	private boolean blur = false;
	private boolean remove = false;
	private boolean face = false;
	private boolean showSegmentation = false;

	private BitSet brainMaskMin;
	private BitSet brainMaskMax;
	
    /**
     * Construct the face anonymizer, but do not run it yet.
     *
     * @param  srcImg            The image to de-face
     * @param  faceDirection     the orientation of the patient's face, as determined by the dialog
     */
    public AlgorithmSkullRemoval(ModelImage srcImg, int faceDirection)
    {
    	super( null, srcImg );
    	destImage = (ModelImage) srcImg.clone();
    	destImage.setImageName( srcImage.getImageName() + "_deskull" );
        faceOrientation = faceDirection;
        
    	dimX = srcImage.getExtents().length > 0 ? srcImage.getExtents()[0] : 1;
    	dimY = srcImage.getExtents().length > 1 ? srcImage.getExtents()[1] : 1;
    	dimZ = srcImage.getExtents().length > 2 ? srcImage.getExtents()[2] : 1;
    	

    	brainMaskMin = new BitSet(dimX*dimY*dimZ);
    	brainMaskMax = new BitSet(dimX*dimY*dimZ);
    }
    /**
     * Clean up memory used by the algorithm.
     */
    public void finalize() {
        srcImage = null;
        super.finalize();
    }

    /**
     * Run the de-facing algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("The source image is null");
            finalize();

            return;
        }

        if (srcImage.getNDims() != 3) {
            displayError("The source image must be 3D");
            finalize();

            return;
        }

        try {
            skullRemoval();
        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Skull Removal: unable to allocate enough memory");

            return;
        }
    }
    
    public void setOutputOption( boolean blur, boolean remove, boolean face, boolean showSegmentation )
    {
    	this.blur = blur;
    	this.remove = remove;
    	this.face = face;
    	this.showSegmentation = showSegmentation;
    }

    
    private void skullRemoval()
    {
        fireProgressStateChanged(0, null, "Estimating parameters ...");

        estimateParameters();
        
        fireProgressStateChanged(30);

        useCSFMin = true;
        maskBrain(useCSFMin, brainMaskMin);       
        destImage.setMask(brainMaskMin);
//        if ( showSegmentation )
//        {
//        	ModelImage csfMinImage = (ModelImage)destImageMin.clone();
//        	csfMinImage.setImageName( srcImage.getImageName() + "_csfMin" );
//        	new ViewJFrameImage(csfMinImage);
//        }

        useCSFMin = false;
        maskBrain(useCSFMin, brainMaskMax);       
//        if ( showSegmentation )
//        {
//        	ModelImage csfMaxImage = (ModelImage)destImageMin.clone();
//        	csfMaxImage.setImageName( srcImage.getImageName() + "_csfMax" );
//        csfMaxImage.setMask(brainMaskMax);
//        	new ViewJFrameImage(csfMaxImage);
//        }
        BitSet newMask = fillCSFMax( brainMaskMax );
        BitSet filledCSFMask = null;
        if ( newMask.cardinality() != 0 )
        {     
        	brainMaskMax = newMask;
            
//            if ( showSegmentation )
//            {
//            	ModelImage csfMaxImage = (ModelImage)destImageMax.clone();
//            	csfMaxImage.setImageName( srcImage.getImageName() + "_csfMax_New" );
//            csfMaxImage.setMask(brainMaskMax);
//            	new ViewJFrameImage(csfMaxImage);
//            }
            filledCSFMask = outlineMask(brainMaskMax);
        }
        
        useCSFMin = true;
        VOI seedPoints;
        if ( filledCSFMask != null )
        {
            brainMaskMin.or( filledCSFMask );
//            if ( showSegmentation )
//            {
//            	ModelImage csfMaxImage = (ModelImage)destImageMax.clone();
//            	csfMaxImage.setImageName( srcImage.getImageName() + "_csfMax_Outline" );
//            	csfMaxImage.setMask(brainMaskMin);
//            	new ViewJFrameImage(csfMaxImage);
//            }
        }
    	seedPoints = estimateWhiteMatter(destImage, brainMaskMin); 
    	

    	boolean noMax = false;
    	for ( int i = 0; i < seedPoints.getCurves().size(); i++ )
    	{
    		VOIBase seedVOI = seedPoints.getCurves().elementAt(i);
    		for ( int j = 0; j < seedVOI.size(); j++ )
    		{
    			Vector3f seed = seedVOI.elementAt(j);    		
        		int index = (int) (seed.Z * dimX * dimY + seed.Y * dimX + seed.X);	
        		if ( brainMaskMax.get(index) )
        		{
        			noMax = true;
        		}
    		}
    	}
    	if ( noMax )
    	{
    		gmThresholdMax = max_98P;
            Preferences.debug( "White matter thresholds : " + gmThresholdMin + " " + gmThresholdMax + " " + noMax + "\n", Preferences.DEBUG_ALGORITHM );
            fillWhiteMatter(destImage, brainMaskMin, seedPoints);
    	}
    	else
    	{
    		gmThresholdMin = min_2P;
    		Preferences.debug( "White matter thresholds : " + gmThresholdMin + " " + gmThresholdMax + " " + noMax + "\n", Preferences.DEBUG_ALGORITHM );
            fillWhiteMatter(destImage, brainMaskMin, seedPoints);
    	}
        
//        if ( showSegmentation )
//        {
//    		destImage.calcMinMax();
//        	ModelImage whiteMatterImage = (ModelImage)destImage.clone();
//        	whiteMatterImage.setImageName( srcImage.getImageName() + "_whiteMatter" );
//        	new ViewJFrameImage(whiteMatterImage);
//        }
        fireProgressStateChanged(60);                
        
        
        calculateRadius(destImage);
        
        initSphere(destImage, noMax);
        
        fireProgressStateChanged(90);
        
        
    	if ( blur || remove )
    	{
    		subtractMask( destImage );
        	destImage.setImageName( srcImage.getImageName() + "_deskull" );
				
			if ( blur )
			{
//				ModelImage blurOutside = randomize( destImage, 5 );	        				
//				 Blur face and add it back to the image:				
		        int index = srcImage.getExtents()[2] / 2;
		        float xRes = srcImage.getFileInfo(index).getResolutions()[0];
		        float zRes = srcImage.getFileInfo(index).getResolutions()[2];

		        float correction = xRes / zRes;

	            final float[] sigmas = {5, 5, 5*correction};
	    		ModelImage blurOutside = new ModelImage(srcImage.getType(), srcImage.getExtents(),
                        JDialogBase.makeImageName(srcImage.getImageName(), "_blur"));
	            AlgorithmGaussianBlurSep gaussianBlurSepAlgo = new AlgorithmGaussianBlurSep(srcImage, sigmas, true, false);
	            gaussianBlurSepAlgo.setRunningInSeparateThread(false);
	            gaussianBlurSepAlgo.run();

                try {
                	blurOutside.importData(0, gaussianBlurSepAlgo.getResultBuffer(), true);    	            
    	            addBlur( blurOutside, destImage );
    	        	destImage.setImageName( srcImage.getImageName() + "_deskull_blur" );
                	blurOutside.disposeLocal();
                } catch (final IOException e) {
                	blurOutside.disposeLocal();
                    MipavUtil.displayError("Algorithm Gausssian Blur importData: Image(s) Locked.");
                    return;
                }
			}
    	}
    	
    	if ( showSegmentation )
    	{
//        	short id = (short) destImage.getVOIs().getUniqueID();
//        	VOI centerGravity = new VOI(id, "centerGravity", VOI.POINT, 1f );
//        	centerGravity.importPoint(cog);
//        	destImage.registerVOI(centerGravity);
//        	centerGravity.setColor( Color.green );    	

    		destImage.resetVOIs();
        	destImage.setImageName( srcImage.getImageName() + "_outsideMask" );
        	new ViewJFrameImage(destImage);
    	}
    	if ( !showSegmentation )
    	{
    		destImage.resetVOIs();
    		destImage.clearMask();
    		new ViewJFrameImage( destImage );
    	}
        fireProgressStateChanged(100);
        
        setCompleted(true);	
    }
    
    private void calculateRadius( ModelImage image )
    {   	
    	float[] res = image.getResolutions(0);
    	Vector3f cog2 = new Vector3f(cog);
    	cog2.scale(res[0], res[1], res[2]);
    	Vector3f pos = new Vector3f();
    	int maxX = -1, maxY = -1, maxZ = -1;
    	for ( int z = 0; z < dimZ; z++ )
    	{
    		for ( int y = 0; y < dimY; y++ )
    		{
    			for ( int x = 0; x < dimX; x++ )
    			{
					int index = z * dimX * dimY + y * dimX + x;
    				if ( image.getMask().get(index) )
    				{
    					float value = image.getFloat(x,y,z);
    					if ( (value > wmThreshold) )
    					{
    						pos.X = x;
    						pos.Y = y;
    						pos.Z = z;
    						pos.scale(res[0], res[1], res[2]);
    						float distance = pos.distance( cog2 );
    						
    						if ( distance > maxRadius )
    						{
    							maxRadius = distance;
    							maxX = x;
    							maxY = y;
    							maxZ = z;
    						}
    					}
    				}
    			}
    		}
    	}
    	
    	if ( (maxX == -1) || (maxY == -1) || (maxZ == -1))
    	{
    		MipavUtil.displayError("Unable to segment image " + srcImage.getImageName());
    		return;
    	}

		float value = image.getFloat(maxX,maxY,maxZ);
    	Preferences.debug( "Max Radius " + maxRadius + " location = " + maxX + " " + maxY + " " + maxZ  + "  value = " + value + "\n", Preferences.DEBUG_ALGORITHM );
    	maxRadius = Math.min( maxRadius, Math.min( dimX, Math.min(dimY, dimZ) ) );
    	maxRadius /= 2f;    	   	
    }
    

	/**
	 * Initialize the scale factors. Based on the ModelImage Volume.
	 */
	private void initSphere(ModelImage image, boolean noMax) 
	{			
		TriMesh sphere = createMesh( image.getMask() );
		if ( sphere != null )
		{
			TriMesh convexHullMesh = convexHull(sphere);

			BitSet meshCHMask = createMask( convexHullMesh, false, "_ch_mask" );

			TriMesh kMesh = createMesh( meshCHMask );
			if ( kMesh != null )
			{
				AlgorithmBrainExtractor betAlg = new AlgorithmBrainExtractor( destImage, faceOrientation, kMesh, 
						(int)getMedianIntensity( destImage, meshCHMask ), cog);
				betAlg.setExtractPaint(true);
				betAlg.extractBrain();
				betAlg = null;
				
				TriMesh convexHullBrain = convexHull(kMesh);
				if ( convexHullBrain != null )
				{
					destImage.setMask( createMask( convexHullBrain, false, "ch_bet_mask" ) );
					kMesh = convexHullBrain;
				}
				
				if ( face | showSegmentation )
				{
					Plane3f clipPlane = new Plane3f( getPlaneNormal( faceOrientation ), getPlaneCenter( faceOrientation ) );
					float minDist = Float.MAX_VALUE;
					int minIndex = -1;
					for ( int i = 0; i < kMesh.VBuffer.GetVertexQuantity(); i++ )
					{
						float distance = clipPlane.DistanceTo( kMesh.VBuffer.GetPosition3(i) );
						if ( distance < minDist )
						{
							minDist = distance;
							minIndex = i;
						}
						if ( distance < 0 )
						{
							System.err.println( "Plane error " + kMesh.VBuffer.GetPosition3(i) );
						}
					}
//					System.err.println( minIndex  + " " + minDist );
					if ( minIndex != -1 )
					{
						clipPlane = new Plane3f( getPlaneNormal(faceOrientation), kMesh.VBuffer.GetPosition3(minIndex) );
						BitSet clipMask = createMask( clipPlane );
						if ( showSegmentation )
						{
							ModelImage outlineMaskImage = (ModelImage)destImage.clone();
							outlineMaskImage.setMask(clipMask);
							outlineMaskImage.setImageName( destImage.getImageName() + "_ClipMask" );  
							outlineMaskImage.resetVOIs();      	
							new ViewJFrameImage(outlineMaskImage);
						}
						if ( face )
						{
							destImage.setMask(clipMask);
						}
					}
				}
			}
		}
		if ( !face )
		{
	        destImage.getMask().flip(0, dimX*dimY*dimZ);
		}
	}
	
    
    private TriMesh removeUnusedVertices( TriMesh kMesh )
    {
    	int[] aiIndex = kMesh.IBuffer.GetData();
    	Vector<Vector3f> vertexList = new Vector<Vector3f>();
    	Vector<Integer> triList = new Vector<Integer>();
    	int indexCount = 0;
    	for ( int i = 0; i < aiIndex.length; i++ )
    	{
    		Vector3f kPos = kMesh.VBuffer.GetPosition3(aiIndex[i]);
    		if ( !vertexList.contains( kPos ) )
    		{
    			vertexList.add( kPos );
    			triList.add( indexCount++ );
    		}
    		else
    		{
    			triList.add( vertexList.indexOf( kPos ) );
    		}
    	}
    	if ( vertexList.size() == kMesh.VBuffer.GetVertexQuantity() )
    	{
//    		System.err.println( "All vertices used" );
    		return kMesh;
    	}
    	int[] aiNewIndex  = new int[ triList.size() ];
    	for ( int i = 0; i < triList.size(); i++ )
    	{
    		aiNewIndex[i] = triList.elementAt(i);
    	}
    	IndexBuffer kIBuffer = new IndexBuffer( aiNewIndex );
    	VertexBuffer kVBuffer = new VertexBuffer( vertexList );
    	return new TriMesh( kVBuffer, kIBuffer );
    }

	
    private void saveMesh(TriMesh mesh, final boolean flip, final String kName) throws IOException {
        TransMatrix dicomMatrix = null;
        TransMatrix inverseDicomMatrix = null;
        // double[][] inverseDicomArray = null;
        float[] coord;
        float[] tCoord;
        int i;


    	float[] res = srcImage.getResolutions(0);
		int numVertices = mesh.VBuffer.GetVertexQuantity();
        float[] startLocation = srcImage.getFileInfo()[0].getOrigin();
        int[] direction = MipavCoordinateSystems.getModelDirections(srcImage);
        
        Vector3f[] positions = new Vector3f[numVertices];
        if (srcImage.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL)) {

            // Get the DICOM transform that describes the transformation from
            // axial to this image orientation
            dicomMatrix = srcImage.getMatrix();
            inverseDicomMatrix = new TransMatrix(srcImage.getMatrix());
            inverseDicomMatrix.Inverse();
            // inverseDicomArray = inverseDicomMatrix.getMatrix();
            // inverseDicomMatrix = null;
            coord = new float[3];
            tCoord = new float[3];

            for (i = 0; i < numVertices; i++) {
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
            	positions[i] = pos;
            }
        } // if (image.getFileInfo()[0].getTransformID() ==
        else {

            for (i = 0; i < numVertices; i++) {
            	Vector3f pos = mesh.VBuffer.GetPosition3(i);
            	pos.X = (pos.X * res[0] * direction[0]) + startLocation[0];
            	pos.Y = (pos.Y * res[1] * direction[1]) + startLocation[1];
            	pos.Z = (pos.Z * res[2] * direction[2]) + startLocation[2];
            	positions[i] = pos;
            }
        } // else


        float[] box = new float[3];
        box[0] = (dimX - 1) * res[0];
        box[1] = (dimY - 1) * res[1];
        box[2] = (dimZ - 1) * res[2];
        
        FileSurface_WM.save(kName, mesh, 0, new VertexBuffer(positions), flip, direction, startLocation, box, inverseDicomMatrix);
    }


	private void createWhiteMatterMesh( ModelImage image, TriMesh sphere )
	{
		int numVertices = sphere.VBuffer.GetVertexQuantity();
		
		for ( int i = 0; i < numVertices; i++ )
		{
			Vector3f pt = sphere.VBuffer.GetPosition3(i);
			Vector3f dir = Vector3f.sub( pt, cog );
			dir.normalize();
			
			Vector3f lastWMPt = new Vector3f(pt);
			boolean found = false;
			while ( inBounds(pt) )
			{
				int index = (int) ((int)pt.Z * dimX * dimY + (int)pt.Y * dimX + (int)pt.X);
				if ( image.getMask().get(index) )
				{
					lastWMPt.copy(pt);
					found = true;
				}
				pt.add(dir);
			}
			pt.copy(lastWMPt);
			if ( !found )
			{
				dir.neg();
				pt.copy(lastWMPt);
				Vector3f checkDir1 = Vector3f.sub( lastWMPt, cog ); checkDir1.normalize();
				Vector3f checkDir2 = Vector3f.sub( pt, cog ); checkDir2.normalize();
    			while ( inBounds(pt) && (checkDir1.dot(checkDir2) > 0) )
    			{
					int index = (int) ((int)pt.Z * dimX * dimY + (int)pt.Y * dimX + (int)pt.X);
    				if ( image.getMask().get(index) )
    				{
    					lastWMPt.copy(pt);
    					break;
    				}
    				Vector3f temp = Vector3f.add(pt,  dir);
    				checkDir2 = Vector3f.sub( temp, cog ); checkDir2.normalize();
    				if ( inBounds(temp) && (checkDir1.dot(checkDir2) > 0) )
    				{
    					pt.add(dir);
    				}
    			}    				
			}
			sphere.VBuffer.SetPosition3(i, pt);
		}
		
		sphere.UpdateMS(true);
	}
	
	
	

	private TriMesh createSkullMesh( ModelImage image, TriMesh sphere, boolean maxFound )
	{		
		int numVertices = sphere.VBuffer.GetVertexQuantity();
		TriMesh skull = new TriMesh( new VertexBuffer(sphere.VBuffer), new IndexBuffer( sphere.IBuffer ) );		
		skull.AttachGlobalState(new MaterialState());

    	float csfThresholdMin = .1f * (max_98P - min_2P);
    	float csfThresholdMax = .9f * (max_98P - min_2P);
		
		
		for ( int i = 0; i < numVertices; i++ )
		{
			Vector3f pt = sphere.VBuffer.GetPosition3(i);
//			Vector3f dir = Vector3f.sub( pt, cog );
			Vector3f dir = sphere.VBuffer.GetNormal3(i);
			dir.normalize();

			Vector3f startPt = new Vector3f(pt);
			Vector3f maxPt = new Vector3f();
			Vector3f minPt = new Vector3f();
			boolean minFound = false;
			while ( inBounds(pt) )
			{
				float value = image.getFloat( (int)pt.X, (int)pt.Y, (int)pt.Z );
				if ( !maxFound && (value > csfThresholdMax) )
				{
					skull.VBuffer.SetPosition3(i, pt);
					maxFound = true;
					maxPt.copy(pt);
				}
				if ( maxFound && !minFound && (value < csfThresholdMin) )
				{
					skull.VBuffer.SetPosition3(i, pt);
					minFound = true;
					minPt.copy(pt);
				}
				if ( maxFound && minFound && ((value > csfThresholdMax ) || minPt.distance(pt) > 5) )
				{
					skull.VBuffer.SetPosition3(i, pt);
					break;
				}
				pt.add(dir);
			}
		}
		
		skull.UpdateMS(true);
		return skull;
	}
	
	    
    private boolean inBounds( Vector3f pt )
    {
		if ( (pt.X >= 0) && (pt.X < dimX) && (pt.Y >= 0) && (pt.Y < dimY) && (pt.Z >= 0) && (pt.Z < dimZ) )
		{
			return true;
		}
		return false;
    }

    
    private boolean inBounds( int x, int y, int z )
    {
		if ( (x >= 0) && (x < dimX) && (y >= 0) && (y < dimY) && (z >= 0) && (z < dimZ) )
		{
			return true;
		}
		return false;
    }
    
    private void addBlur( ModelImage blurImage, ModelImage destImage )
    {    	
    	for ( int z = 0; z < dimZ; z++ )
    	{
    		for ( int y = 0; y < dimY; y++ )
    		{
    			for ( int x = 0; x < dimX; x++ )
    			{
					int index = z * dimX * dimY + y * dimX + x;
					if ( destImage.getMask().get(index) )
					{
						destImage.set(x,y,z, blurImage.getFloat(x,y,z) );
					}
    			}
    		}
    	}
    	
    }
    
    private void maskBrain( Boolean useMin, BitSet mask )
    {

    	for ( int z = 0; z < dimZ; z++ )
    	{
    		for ( int y = 0; y < dimY; y++ )
    		{
    			for ( int x = 0; x < dimX; x++ )
    			{
    				float value = destImage.getFloat(x,y,z);
    				if ( useMin && (value <= csfThresholdMin) )
    				{
    					mask.set( z * dimX*dimY + y * dimX + x );
    				}
    				else if ( !useMin && (value >= csfThresholdMax) )
    				{
    					mask.set( z * dimX*dimY + y * dimX + x );
    				}
    			}
    		}
    	}
    }

    
    private float getMedianIntensity( ModelImage image, BitSet mask )
    {
    	int iIQuantity = 0;
    	float[] intensity = new float[mask.size()];
    	for ( int z = 0; z < dimZ; z++ )
    	{
    		for ( int y = 0; y < dimY; y++ )
    		{
    			for ( int x = 0; x < dimX; x++ )
    			{
    				int index = (int)( z * dimX*dimY + y * dimX + x );
    				if ( mask.get(index) )
    				{
    					float value = image.getFloat(x,y,z);
    					intensity[iIQuantity++] = value;
    				}
    			}
    		}
    	}
        if (iIQuantity != 0) {
            Arrays.sort(intensity, 0, iIQuantity - 1);
        }

        return intensity[iIQuantity / 2];
    }
    
    private void fillWhiteMatter( ModelImage image, BitSet csfMask, VOI seedPoints )
    {
    	if ( seedPoints == null )
    	{
    		return;
    	}
    	if ( seedPoints.getCurves().size() <= 0 )
    	{
    		return;
    	}
    	int length = dimX * dimY * dimZ;
    	BitSet visited = new BitSet(length);
    	BitSet whiteMatter = new BitSet(length);
    	
    	Vector<Vector3f> seedList = new Vector<Vector3f>();
    	for ( int i = 0; i < seedPoints.getCurves().size(); i++ )
    	{
    		VOIBase seedVOI = seedPoints.getCurves().elementAt(i);
    		for ( int j = 0; j < seedVOI.size(); j++ )
    		{
    			Vector3f seed = seedVOI.elementAt(j);    		
        		int index = (int) (seed.Z * dimX * dimY + seed.Y * dimX + seed.X);	
    			visited.set(index);
				whiteMatter.set(index);
    			seedList.add(seed);
    		}
    	}    	
    	
		fill( image, seedList, visited, whiteMatter, csfMask );
    	
		image.setMask( whiteMatter );
    }
    
    private void fill( ModelImage image, Vector<Vector3f> seedList, BitSet visited, BitSet whiteMatter, BitSet csfMask )
    {
    	while ( seedList.size() > 0 )
    	{
    		Vector3f seed = seedList.remove(0);

    		boolean csfFound = false;
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
    							int index = (int) (z * dimX * dimY + y * dimX + x);

    							if ( csfMask.get(index)  )
    							{
    								csfFound = true;
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
								int index = (int) (z * dimX * dimY + y * dimX + x);

								if ( !visited.get(index) )
								{
									visited.set(index);
									float value = image.getFloat(x, y, z);
									if ( !csfFound )
									{
										if ( (value > gmThresholdMin) && (value < gmThresholdMax) )
										{
											whiteMatter.set(index);
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
    
    
    
    

    private BitSet fillCSFMax( BitSet csfMask )
    {
    	int length = dimX * dimY * dimZ;
    	BitSet visited = new BitSet(length);
    	BitSet newMask = new BitSet(length);
    	Vector<Vector3f> seedList = new Vector<Vector3f>();


		int index = (int) (cog.Z * dimX * dimY + cog.Y * dimX + cog.X);	
        Preferences.debug( "Min Center " + brainMaskMax.get(index) + "\n", Preferences.DEBUG_ALGORITHM );
        if ( csfMask.get(index) )
        {
			visited.set(index);
			newMask.set(index);
        	seedList.add( new Vector3f(cog) );
        }
        
        index = (int) (cogMax.Z * dimX * dimY + cogMax.Y * dimX + cogMax.X);	
        Preferences.debug( "Max Center " + brainMaskMax.get(index) + "\n", Preferences.DEBUG_ALGORITHM );
        if ( csfMask.get(index) )
        {
			visited.set(index);
			newMask.set(index);
        	seedList.add( new Vector3f(cogMax) );
        }
    	    	
        if ( seedList.size() == 0 )
        {
			for ( int z = (int) (cog.Z-5); z <= cog.Z+5; z++ )
			{
				for ( int y = (int) (cog.Y-5); y <= cog.Y+5; y++ )
				{
					for ( int x = (int) (cog.X-5); x <= cog.X+5; x++ )
					{
						if ( (x >= 0) && (x < dimX) && (y >= 0) && (y < dimY) && (z >= 0) && (z < dimZ) )
						{
							index = (int) (z * dimX * dimY + y * dimX + x);
							if ( csfMask.get(index) )
							{
								visited.set(index);
								newMask.set(index);
								seedList.add( new Vector3f(x, y, z) );
							}
						}
					}
				}
			}    	
			for ( int z = (int) (cogMax.Z-5); z <= cogMax.Z+5; z++ )
			{
				for ( int y = (int) (cogMax.Y-5); y <= cogMax.Y+5; y++ )
				{
					for ( int x = (int) (cogMax.X-5); x <= cogMax.X+5; x++ )
					{
						if ( (x >= 0) && (x < dimX) && (y >= 0) && (y < dimY) && (z >= 0) && (z < dimZ) )
						{
							index = (int) (z * dimX * dimY + y * dimX + x);
							if ( csfMask.get(index) )
							{
								visited.set(index);
								newMask.set(index);
								seedList.add( new Vector3f(x, y, z) );
							}
						}
					}
				}
			}    	
        }
    	
		fillCSFMax( seedList, visited, newMask, csfMask );
    	return newMask;
    }
    
    private void fillCSFMax( Vector<Vector3f> seedList, BitSet visited, BitSet newMask, BitSet csfMask )
    {    	
    	
    	

    	while ( seedList.size() > 0 )
    	{
    		Vector3f seed = seedList.remove(0);

    		int x = (int) seed.X;
    		int y = (int) seed.Y;
    		int z = (int) (seed.Z -1);
    		if ( inBounds( x, y, z ) )
    		{
    			int index = (int) (z * dimX * dimY + y * dimX + x);
    			if ( !visited.get(index) )
    			{
    				visited.set(index);
    				if ( csfMask.get(index) )
    				{
    					newMask.set(index);
    					seedList.add( new Vector3f(x, y, z) );
    				}
    			}
    		}

    		x = (int) seed.X;
    		y = (int) seed.Y;
    		z = (int) (seed.Z +1);
    		if ( inBounds( x, y, z ) )
    		{
    			int index = (int) (z * dimX * dimY + y * dimX + x);
    			if ( !visited.get(index) )
    			{
    				visited.set(index);
    				if ( csfMask.get(index) )
    				{
    					newMask.set(index);
    					seedList.add( new Vector3f(x, y, z) );
    				}
    			}
    		}

    		x = (int) seed.X;
    		y = (int) (seed.Y - 1);
    		z = (int) seed.Z;
    		if ( inBounds( x, y, z ) )
    		{
    			int index = (int) (z * dimX * dimY + y * dimX + x);
    			if ( !visited.get(index) )
    			{
    				visited.set(index);
    				if ( csfMask.get(index) )
    				{
    					newMask.set(index);
    					seedList.add( new Vector3f(x, y, z) );
    				}
    			}
    		}

    		x = (int) seed.X;
    		y = (int) (seed.Y + 1);
    		z = (int) seed.Z;
    		if ( inBounds( x, y, z ) )
    		{
    			int index = (int) (z * dimX * dimY + y * dimX + x);
    			if ( !visited.get(index) )
    			{
    				visited.set(index);
    				if ( csfMask.get(index) )
    				{
    					newMask.set(index);
    					seedList.add( new Vector3f(x, y, z) );
    				}
    			}
    		}

    		x = (int) (seed.X - 1);
    		y = (int) seed.Y;
    		z = (int) seed.Z;
    		if ( inBounds( x, y, z ) )
    		{
    			int index = (int) (z * dimX * dimY + y * dimX + x);
    			if ( !visited.get(index) )
    			{
    				visited.set(index);
    				if ( csfMask.get(index) )
    				{
    					newMask.set(index);
    					seedList.add( new Vector3f(x, y, z) );
    				}
    			}
    		}

    		x = (int) (seed.X + 1);
    		y = (int) seed.Y;
    		z = (int) seed.Z;
    		if ( inBounds( x, y, z ) )
    		{
    			int index = (int) (z * dimX * dimY + y * dimX + x);
    			if ( !visited.get(index) )
    			{
    				visited.set(index);
    				if ( csfMask.get(index) )
    				{
    					newMask.set(index);
    					seedList.add( new Vector3f(x, y, z) );
    				}
    			}
    		}
    	}
    	
    	
    	
    	
    	
    	
    	
    	
    	
    	
    	
    	
    	
    	
    	
    	
    	
    	
    	
    	
    	
    	
    	
    	
    	
    	
    	
    	
    	
    	
    	
    	
    	
//    	while ( seedList.size() > 0 )
//    	{
//    		Vector3f seed = seedList.remove(0);
//			for ( int z = (int) (seed.Z-1); z <= seed.Z+1; z++ )
//			{
//				for ( int y = (int) (seed.Y-1); y <= seed.Y+1; y++ )
//				{
//					for ( int x = (int) (seed.X-1); x <= seed.X+1; x++ )
//					{
//						if ( (x >= 0) && (x < dimX) && (y >= 0) && (y < dimY) && (z >= 0) && (z < dimZ) )
//						{
//							if ( x != seed.X || y != seed.Y || z != seed.Z )
//							{
//								int index = (int) (z * dimX * dimY + y * dimX + x);
//
//								if ( !visited.get(index) )
//								{
//									visited.set(index);
//									if ( csfMask.get(index) )
//									{
//										newMask.set(index);
//										seedList.add( new Vector3f(x, y, z) );
//									}
//    							}
//    						}
//    					}
//    				}
//    			}
//    		}
//    	}
    }
    
    
    
    private BitSet fillCSFMin( BitSet csfMask )
    {
    	int length = dimX * dimY * dimZ;
    	BitSet visited = new BitSet(length);
    	BitSet newMask = new BitSet(length);
    	Vector<Vector3f> seedList = new Vector<Vector3f>();


		int index = (int) (cog.Z * dimX * dimY + cog.Y * dimX + cog.X);	
        Preferences.debug( "Min Center " + brainMaskMax.get(index) + "\n", Preferences.DEBUG_ALGORITHM );
        if ( !csfMask.get(index) )
        {
			visited.set(index);
			newMask.set(index);
        	seedList.add( new Vector3f(cog) );
        }
        
        index = (int) (cogMax.Z * dimX * dimY + cogMax.Y * dimX + cogMax.X);	
        Preferences.debug( "Max Center " + brainMaskMax.get(index) + "\n", Preferences.DEBUG_ALGORITHM );
        if ( !csfMask.get(index) )
        {
			visited.set(index);
			newMask.set(index);
        	seedList.add( new Vector3f(cogMax) );
        }
    	    	
        if ( seedList.size() == 0 )
        {
			for ( int z = (int) (cog.Z-5); z <= cog.Z+5; z++ )
			{
				for ( int y = (int) (cog.Y-5); y <= cog.Y+5; y++ )
				{
					for ( int x = (int) (cog.X-5); x <= cog.X+5; x++ )
					{
						if ( (x >= 0) && (x < dimX) && (y >= 0) && (y < dimY) && (z >= 0) && (z < dimZ) )
						{
							index = (int) (z * dimX * dimY + y * dimX + x);
							if ( !csfMask.get(index) )
							{
								visited.set(index);
								newMask.set(index);
								seedList.add( new Vector3f(x, y, z) );
							}
						}
					}
				}
			}    	
			for ( int z = (int) (cogMax.Z-5); z <= cogMax.Z+5; z++ )
			{
				for ( int y = (int) (cogMax.Y-5); y <= cogMax.Y+5; y++ )
				{
					for ( int x = (int) (cogMax.X-5); x <= cogMax.X+5; x++ )
					{
						if ( (x >= 0) && (x < dimX) && (y >= 0) && (y < dimY) && (z >= 0) && (z < dimZ) )
						{
							index = (int) (z * dimX * dimY + y * dimX + x);
							if ( !csfMask.get(index) )
							{
								visited.set(index);
								newMask.set(index);
								seedList.add( new Vector3f(x, y, z) );
							}
						}
					}
				}
			}    	
        }
    	
		fillCSFMin( seedList, visited, newMask, csfMask );
    	return newMask;
    }
    
    private void fillCSFMin( Vector<Vector3f> seedList, BitSet visited, BitSet newMask, BitSet csfMask )
    {    	
    	while ( seedList.size() > 0 )
    	{
    		Vector3f seed = seedList.remove(0);

    		int x = (int) seed.X;
    		int y = (int) seed.Y;
    		int z = (int) (seed.Z -1);
    		if ( inBounds( x, y, z ) )
    		{
    			int index = (int) (z * dimX * dimY + y * dimX + x);
    			if ( !visited.get(index) )
    			{
    				visited.set(index);
    				if ( !csfMask.get(index) )
    				{
    					newMask.set(index);
    					seedList.add( new Vector3f(x, y, z) );
    				}
    			}
    		}

    		x = (int) seed.X;
    		y = (int) seed.Y;
    		z = (int) (seed.Z +1);
    		if ( inBounds( x, y, z ) )
    		{
    			int index = (int) (z * dimX * dimY + y * dimX + x);
    			if ( !visited.get(index) )
    			{
    				visited.set(index);
    				if ( !csfMask.get(index) )
    				{
    					newMask.set(index);
    					seedList.add( new Vector3f(x, y, z) );
    				}
    			}
    		}

    		x = (int) seed.X;
    		y = (int) (seed.Y - 1);
    		z = (int) seed.Z;
    		if ( inBounds( x, y, z ) )
    		{
    			int index = (int) (z * dimX * dimY + y * dimX + x);
    			if ( !visited.get(index) )
    			{
    				visited.set(index);
    				if ( !csfMask.get(index) )
    				{
    					newMask.set(index);
    					seedList.add( new Vector3f(x, y, z) );
    				}
    			}
    		}

    		x = (int) seed.X;
    		y = (int) (seed.Y + 1);
    		z = (int) seed.Z;
    		if ( inBounds( x, y, z ) )
    		{
    			int index = (int) (z * dimX * dimY + y * dimX + x);
    			if ( !visited.get(index) )
    			{
    				visited.set(index);
    				if ( !csfMask.get(index) )
    				{
    					newMask.set(index);
    					seedList.add( new Vector3f(x, y, z) );
    				}
    			}
    		}

    		x = (int) (seed.X - 1);
    		y = (int) seed.Y;
    		z = (int) seed.Z;
    		if ( inBounds( x, y, z ) )
    		{
    			int index = (int) (z * dimX * dimY + y * dimX + x);
    			if ( !visited.get(index) )
    			{
    				visited.set(index);
    				if ( !csfMask.get(index) )
    				{
    					newMask.set(index);
    					seedList.add( new Vector3f(x, y, z) );
    				}
    			}
    		}

    		x = (int) (seed.X + 1);
    		y = (int) seed.Y;
    		z = (int) seed.Z;
    		if ( inBounds( x, y, z ) )
    		{
    			int index = (int) (z * dimX * dimY + y * dimX + x);
    			if ( !visited.get(index) )
    			{
    				visited.set(index);
    				if ( !csfMask.get(index) )
    				{
    					newMask.set(index);
    					seedList.add( new Vector3f(x, y, z) );
    				}
    			}
    		}
    	}
    }
    
    
    
    
    
    private VOI estimateWhiteMatter( ModelImage image, BitSet mask )
    {
    	int cubeSize = 5;
    	int cubeHalf = cubeSize/2;
    	
    	Vector<Vector2d> cubeIntensities = new Vector<Vector2d>();
    	Vector<Vector3f> cubeCenters = new Vector<Vector3f>();
    	for ( int z = 0; z < dimZ - cubeSize; z++ )
    	{
    		for ( int y = 0; y < dimY - cubeSize; y++ )
    		{
    			for ( int x = 0; x < dimX - cubeSize; x++ )
    			{
    				boolean csfFound = false;
    				int index;
    				int count = 0;
    				double meanIntensity = 0;
    				for ( int z2 = z; z2 < (z+cubeSize); z2++ )
    				{
        				for ( int y2 = y; y2 < (y+cubeSize); y2++ )
        				{
            				for ( int x2 = x; x2 < (x+cubeSize); x2++ )
            				{
            					if ( (x2 < dimX) && (y2 < dimY) && (z2 < dimZ) )
            					{
            						float value = srcImage.getFloat(x2,y2,z2);
            						meanIntensity += value;
            						count++;
            						index = z2 * dimX*dimY + y2 * dimX + x2;
            						if ( mask.get(index) )
            						{
            							csfFound = true;
            						}
            					}
            				}
        				}    					
    				}
    				if ( !csfFound )
    				{
    					meanIntensity /= (float)count;

    					count = 0;
    					double meanVariance = 0;
    					for ( int z2 = z; z2 < (z+cubeSize); z2++ )
    					{
    						for ( int y2 = y; y2 < (y+cubeSize); y2++ )
    						{
    							for ( int x2 = x; x2 < (x+cubeSize); x2++ )
    							{
    								if ( (x2 < dimX) && (y2 < dimY) && (z2 < dimZ) )
    								{
    									float value = srcImage.getFloat(x2,y2,z2);
    									meanVariance += ((value - meanIntensity) * (value - meanIntensity));
    									count++;
    								}
    							}
    						}    					
    					}
    					meanVariance /= (float)count;
    					cubeIntensities.add(new Vector2d( meanIntensity, meanVariance) );

    					Vector3f center = new Vector3f( x + cubeHalf, y + cubeHalf, z + cubeHalf );
    					cubeCenters.add(center);
    				}
    			}
    		}
    	}
    	
    	Vector2d[] sortedIntensities = new Vector2d[cubeIntensities.size()];
    	for ( int i = 0; i < cubeIntensities.size(); i++ )
    	{
    		sortedIntensities[i] = cubeIntensities.elementAt(i);
    	}
    	Arrays.sort( sortedIntensities );

//    	System.err.println( "Intensity range =    " + sortedIntensities[0].X + "    " +  sortedIntensities[cubeIntensities.size()-1].X );
//    	System.err.println( "Intensity range = " + (sortedIntensities[0].X - sortedIntensities[cubeIntensities.size()-1].X) );
//    	System.err.println( "Variance range =    " + sortedIntensities[0].Y + "    " + sortedIntensities[cubeIntensities.size()-1].Y );
//    	System.err.println( "Variance range = " + (sortedIntensities[0].Y - sortedIntensities[cubeIntensities.size()-1].Y) );
    	
    	int minIndex = -1;
    	double minVar = Double.MAX_VALUE;
    	for ( int i = 0; i < sortedIntensities.length; i++ )
    	{    		
    		if ( useCSFMin && (sortedIntensities[i].X > csfThresholdMin) && ( sortedIntensities[i].Y < minVar) )
    		{
    			minVar = sortedIntensities[i].Y;
    			minIndex = i;
    		}
    		else if ( !useCSFMin && (sortedIntensities[i].X < csfThresholdMax) && ( sortedIntensities[i].Y < minVar) )
    		{
    			minVar = sortedIntensities[i].Y;
    			minIndex = i;
    		}
    	}
    	
   	
    	
    	

    	short id = (short) image.getVOIs().getUniqueID();
    	VOI seedPoints = new VOI(id, "SeedPoints", VOI.POINT, 1f );
    	

    	for ( int i = 0; i < cubeCenters.size(); i++ )
    	{
    		if ( useCSFMin && (cubeIntensities.elementAt(i).X > csfThresholdMin) && 
    				(cubeIntensities.elementAt(i).Y == sortedIntensities[minIndex].Y) )
    		{   			
    			seedPoints.importPoint(cubeCenters.elementAt(i));

    			Preferences.debug( "Seed point: " + cubeCenters.elementAt(i) + "       intensity = " + cubeIntensities.elementAt(i).X + " variance = " + cubeIntensities.elementAt(i).Y + "\n", Preferences.DEBUG_ALGORITHM );
//    			System.err.println( "Seed point: " + cubeCenters.elementAt(i) + "       intensity = " + cubeIntensities.elementAt(i).X + " variance = " + cubeIntensities.elementAt(i).Y );
    		}
    		else if ( !useCSFMin && (cubeIntensities.elementAt(i).X < csfThresholdMax) && 
    				(cubeIntensities.elementAt(i).Y == sortedIntensities[minIndex].Y) )
    		{   			
    			seedPoints.importPoint(cubeCenters.elementAt(i));

    			Preferences.debug( "Seed point: " + cubeCenters.elementAt(i) + "       intensity = " + cubeIntensities.elementAt(i).X + " variance = " + cubeIntensities.elementAt(i).Y + "\n", Preferences.DEBUG_ALGORITHM );
//    			System.err.println( "Seed point: " + cubeCenters.elementAt(i) + "       intensity = " + cubeIntensities.elementAt(i).X + " variance = " + cubeIntensities.elementAt(i).Y );
    		}
    	}

    	image.registerVOI(seedPoints);
    	seedPoints.setColor( Color.red );
    	
    	wmThreshold = (float) sortedIntensities[minIndex].X;
//    	double variance = sortedIntensities[minIndex].Y;
//    	double standardDeviation = Math.sqrt(variance);
//    	gmThresholdMin = (float) (wmThreshold - 7*standardDeviation);
//    	gmThresholdMax = (float) (wmThreshold + 7*standardDeviation);
    	gmThresholdMin = Math.max( csfThresholdMin, 0.36f * wmThreshold );
    	gmThresholdMax = Math.min( csfThresholdMax, (float) (wmThreshold + .75 * (max_98P - wmThreshold)));

    	Preferences.debug( "White matter threshold = " + wmThreshold + " variance = " + sortedIntensities[minIndex].Y + "\n", Preferences.DEBUG_ALGORITHM );
    	Preferences.debug( "Gray matter threshold = " + gmThresholdMin + "\n", Preferences.DEBUG_ALGORITHM );
    	Preferences.debug( "Gray matter threshold = " + gmThresholdMax + "\n", Preferences.DEBUG_ALGORITHM );
    	
    	
    	Preferences.debug( "Number of seed points " + seedPoints.getCurves().size() + "\n", Preferences.DEBUG_ALGORITHM );

    	cubeIntensities.clear();
    	cubeCenters.clear();
    	cubeIntensities = null;
    	cubeCenters = null;
    	sortedIntensities = null;
    	return seedPoints;
    }
        
    private void estimateParameters()
    {
    	double min = srcImage.getMin();
    	double max = srcImage.getMax();
    	    	
    	
    	// Calculate the histogram:
    	HashMap<Float, Integer> map = new HashMap<Float, Integer>();
    	for ( int z = 0; z < dimZ; z++ )
    	{
    		for ( int y = 0; y < dimY; y++ )
    		{
    			for ( int x = 0; x < dimX; x++ )
    			{
    				float value = srcImage.getFloat(x,y,z);
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
    	int count_98P = (int) (0.98 * totalCount);
    	
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
    	boolean maxFound = false;
    	for ( int i = 0; i < counts.length; i++ )
    	{
    		counts[i] = map.get( keyArray[i] );
    		runningCount += counts[i];

    		if ( !minFound && (runningCount >= count_2P) )
    		{
    			min_2P = keyArray[i];
    			minFound = true;
    		}
    		if ( !maxFound && (runningCount >= count_98P) )
    		{
    			max_98P = keyArray[i];
    			maxFound = true;
    		}
    	}
    	
    	Preferences.debug( "Image min = " + min + " max = " + max + "\n", Preferences.DEBUG_ALGORITHM );
    	Preferences.debug( "Robust min = " + min_2P + " robust max = " + max_98P + "\n", Preferences.DEBUG_ALGORITHM );

    	csfThresholdMin = .3f * (max_98P - min_2P);
    	csfThresholdMax = .7f * (max_98P - min_2P);
    	Preferences.debug( "CSF Threshold = " + csfThresholdMin + "   " + csfThresholdMax + "\n", Preferences.DEBUG_ALGORITHM );

    	double weightSumMin = 0;
    	double weightSumMax = 0;
    	Vector3f posMin = new Vector3f();
    	Vector3f posMax = new Vector3f();
    	for ( int z = 0; z < dimZ; z++ )
    	{
    		for ( int y = 0; y < dimY; y++ )
    		{
    			for ( int x = 0; x < dimX; x++ )
    			{
    				float value = srcImage.getFloat(x,y,z);
    				if ( value < min_2P )
    				{
    					value = min_2P;
    				}
    				else if ( value > max_98P )
    				{
    					value = max_98P;    					
    				}
    				else
    				{
    					if ( value > csfThresholdMin )
    					{
    						// value is between min and max and greater than csf threshold
    						posMin.X += (x * value);
    						posMin.Y += (y * value);
    						posMin.Z += (z * value);

    						weightSumMin += value;
    					}
    					if ( value < csfThresholdMax )
    					{
    						// value is between min and max and greater than csf threshold
    						posMax.X += (x * value);
    						posMax.Y += (y * value);
    						posMax.Z += (z * value);

    						weightSumMax += value;
    					}
    				}
    				destImage.set(x, y, z, value );
    			}
    		}
    	}

    	float scaleMin = (float) (1f/weightSumMin);
    	posMin.scale( scaleMin );
    	posMin.X = Math.round(posMin.X);
    	posMin.Y = Math.round(posMin.Y);
    	posMin.Z = Math.round(posMin.Z);
    	short id = (short) destImage.getVOIs().getUniqueID();
    	VOI centerMin = new VOI(id, "centerMin", VOI.POINT, 1f );
    	centerMin.importPoint(posMin);
    	destImage.registerVOI(centerMin);
    	centerMin.setColor( Color.green );

    	float scaleMax = (float) (1f/weightSumMax);
    	posMax.scale( scaleMax );
    	posMax.X = Math.round(posMax.X);
    	posMax.Y = Math.round(posMax.Y);
    	posMax.Z = Math.round(posMax.Z);
    	id = (short) destImage.getVOIs().getUniqueID();
    	VOI centerMax = new VOI(id, "centerMax", VOI.POINT, 1f );
    	centerMax.importPoint(posMax);
    	destImage.registerVOI(centerMax);
    	centerMax.setColor( Color.magenta );
    	


		float value = destImage.getFloat((int)posMin.X, (int)posMin.Y, (int)posMin.Z);
        Preferences.debug( "CSF min = " + posMin + "   value = " + value + "\n", Preferences.DEBUG_ALGORITHM );
    	value = destImage.getFloat((int)posMax.X, (int)posMax.Y, (int)posMax.Z);
        Preferences.debug( "CSF max = " + posMax + "   value = " + value + "\n", Preferences.DEBUG_ALGORITHM );

    	cog = new Vector3f(posMin);
    	cogMax = new Vector3f(posMax);
//    	short id = (short) destImage.getVOIs().getUniqueID();
//    	VOI centerGravity = new VOI(id, "centerGravity", VOI.POINT, 1f );
//    	centerGravity.importPoint(cog);
//    	destImage.registerVOI(centerGravity);
//    	centerGravity.setColor( Color.green );    	
//    	Preferences.debug( "Center of gravity " + cog + "\n", Preferences.DEBUG_ALGORITHM );   	    	
    }

    private VOIContour circleX( float scale, float maxRadius, Vector3f cog, float x )
    {
    	float[] res = srcImage.getResolutions(0);
    	float zRatio = res[1] / res[2];
		VOIContour sphereContour = new VOIContour( false, true );
		float radialSamples = 900;
		for ( int i = 0; i < radialSamples; i++ )
		{
			Vector3f pt = new Vector3f( x, (float)(cog.Y + scale*maxRadius * Math.cos( Math.PI * 2.0 * i/radialSamples )),
					(float)(cog.Z + zRatio * scale*maxRadius * Math.sin( Math.PI * 2.0 * i/radialSamples)) );
			sphereContour.add(pt);
		}
		return sphereContour;
    }

    private VOIContour circleY( float scale, float maxRadius, Vector3f cog, float y )
    {
    	float[] res = srcImage.getResolutions(0);
    	float zRatio = res[0] / res[2];
		VOIContour sphereContour = new VOIContour( false, true );
		float radialSamples = 900;
		for ( int i = 0; i < radialSamples; i++ )
		{
			Vector3f pt = new Vector3f( (float)(cog.X + scale*maxRadius * Math.cos( Math.PI * 2.0 * i/radialSamples )), y,
					(float)(cog.Z + zRatio * scale*maxRadius * Math.sin( Math.PI * 2.0 * i/radialSamples)) );
			sphereContour.add(pt);
		}
		return sphereContour;
    }
    
    private VOIContour circleZ( float scale, float maxRadius, Vector3f cog, float z )
    {
    	float[] res = srcImage.getResolutions(0);
    	float yRatio = res[0] / res[1];
		VOIContour sphereContour = new VOIContour( false, true );
		float radialSamples = 900;
		for ( int i = 0; i < radialSamples; i++ )
		{
			Vector3f pt = new Vector3f( (float)(cog.X + scale*maxRadius * Math.cos( Math.PI * 2.0 * i/radialSamples )),
					(float)(cog.Y + yRatio * scale*maxRadius * Math.sin( Math.PI * 2.0 * i/radialSamples)), z );
			sphereContour.add(pt);
		}
		return sphereContour;
    }
    
    

    /**
     * Creates a BitSet mask that is the volume enclosed by the triangle mesh.
     * This function is not accurate if the mesh is not closed, however it will still return a mask.
     * @return
     */
    private BitSet createVolumeMask( final TriMesh kMesh, BitSet wmMask )
    {    	
    	final BoxBV kBoundingBox = new BoxBV(kMesh.VBuffer);
    	
    	Vector3f[] kBoxCorners = new Vector3f[8];
    	kBoundingBox.GetBox().ComputeVertices( kBoxCorners );
    	Vector3f kMaxBB = new Vector3f( -Float.MAX_VALUE, -Float.MAX_VALUE, -Float.MAX_VALUE );
    	Vector3f kMinBB = new Vector3f(  Float.MAX_VALUE,  Float.MAX_VALUE,  Float.MAX_VALUE );
    	for ( int i = 0; i < kBoxCorners.length; i++ )
    	{
        	kMaxBB.max( kBoxCorners[i] );
        	kMinBB.min( kBoxCorners[i] );
    	}
            	
    	final BitSet mask = new BitSet();
    	
    	final Vector3f[] directions = new Vector3f[5];
    	directions[0] = new Vector3f( (float)Math.random(), (float)Math.random(), (float)Math.random() );
    	directions[1] = new Vector3f( -(float)Math.random(), (float)Math.random(), (float)Math.random() );
    	directions[2] = new Vector3f( (float)Math.random(), -(float)Math.random(), (float)Math.random() );
    	directions[3] = new Vector3f( (float)Math.random(), (float)Math.random(), -(float)Math.random() );
    	directions[4] = new Vector3f( -(float)Math.random(), (float)Math.random(), -(float)Math.random() );
    	for ( int i = 0; i < directions.length; i++ )
    	{
    		directions[i].normalize();
    	}
        long startTime = System.currentTimeMillis();
    	

		int xMin = (int)Math.floor(kMinBB.X);
		int yMin = (int)Math.floor(kMinBB.Y);
		int zMin = (int)Math.floor(kMinBB.Z);
		int xMax = (int)Math.ceil(kMaxBB.X);
		int yMax = (int)Math.ceil(kMaxBB.Y);
		int zMax = (int)Math.ceil(kMaxBB.Z);

//        if (Preferences.isMultiThreadingEnabled())
//        {
//        	int nthreads = ThreadUtil.getAvailableCores();
//        	int intervalX = xMax - xMin;
//        	int intervalY = yMax - yMin;
//        	int intervalZ = zMax - zMin;
//            final CountDownLatch doneSignal = new CountDownLatch(nthreads);
//            float stepX = 0;
//            if ( intervalX > intervalY && intervalX > intervalZ )
//            {
//            	stepX = (float)intervalX / (float)nthreads;
//            }
//            float stepY = 0;
//            if ( intervalY > intervalX && intervalY > intervalZ )
//            {
//            	stepY = (float)intervalY / (float)nthreads;
//            }
//            float stepZ = 0;
//            if ( intervalZ > intervalX && intervalZ > intervalY )
//            {
//            	stepZ = (float)intervalZ / (float)nthreads;
//            }
//            if ( stepX == 0 && stepY == 0 && stepZ == 0 )
//            {
//            	stepZ = (float)intervalZ / (float)nthreads;			            	
//            }
//
//            for (int i = 0; i < nthreads; i++) {
//                final int startX = stepX == 0 ? xMin : (int) (xMin + (    i * stepX));
//                final int   endX = stepX == 0 ? xMax : (int) (xMin + ((i+1) * stepX));
//                final int startY = stepY == 0 ? yMin : (int) (yMin + (    i * stepY));
//                final int   endY = stepY == 0 ? yMax : (int) (yMin + ((i+1) * stepY));
//                final int startZ = stepZ == 0 ? zMin : (int) (zMin + (    i * stepZ));
//                final int   endZ = stepZ == 0 ? zMax : (int) (zMin + ((i+1) * stepZ));
//                System.err.println( startX + " " + endX + "      " + startY + " " + endY + "      " + startZ + "  " + endZ );
//                final Runnable task = new Runnable() {
//                    public void run() {
//                    	calcVolumeMask( kMesh, kBoundingBox, mask, null, directions, startX, endX, startY, endY, startZ, endZ,
//                        		dimX, dimY );
//                        doneSignal.countDown();
//                    }
//                };
//
//                ThreadUtil.mipavThreadPool.execute(task);
//            }
//            try {
//                doneSignal.await();
//            } catch (final InterruptedException e) {
//                e.printStackTrace();
//            }
//        }
//        else
        {
        	calcVolumeMask( kMesh, wmMask, kBoundingBox, mask, directions, xMin, xMax, yMin, yMax, zMin, zMax );
        }
    	
    	
        
        long now = System.currentTimeMillis();
        double elapsedTime = (double) (now - startTime);

        // if elasedTime is invalid, then set it to 0
        if (elapsedTime <= 0) {
            elapsedTime = (double) 0.0;
        }

        double timeinSec =  (double) (elapsedTime / 1000.0); // return in seconds!!
        
//        System.err.println( "Elapsed time: " + timeinSec );
		//kOutputImage.calcMinMax();
		//new ViewJFrameImage( kOutputImage );
		return mask;
    }
    
    private void calcVolumeMask( final TriMesh kMesh, BitSet wmMask, final BoxBV kBoundingBox,
    		final BitSet mask,     		 
    		final Vector3f[] directions, 
    		final int xMin, final int xMax,
    		final int yMin, final int yMax, 
    		final int zMin, final int zMax )
    {
    	Vector3f kTest = new Vector3f();
    	Line3f[] akLines = new Line3f[directions.length];
		for ( int i = 0; i < directions.length; i++ )
		{
			akLines[i] = new Line3f( Vector3f.ONE, directions[i] );
		}
    	
//    	System.err.println( kMesh.GetTriangleQuantity() );
    	for ( int z = zMin; z <= zMax; z++ )
    	{
//    		System.err.println( z );
    		for ( int y = yMin; y <= yMax; y++ )
    		{
    			for ( int x = xMin; x <= xMax; x++ )
    			{
    				int index = z * dimX * dimY + y * dimX + x;
    				if ( wmMask.get(index) )
    				{
    					mask.set(index);
    				}
    				else
    				{
    					kTest.set(x,y,z);

    					if ( kBoundingBox.Contains( kTest ) )
    					{
    						for ( int i = 0; i < directions.length; i++ )
    						{
    							akLines[i].Origin = kTest;
    						}
    						if ( testIntersections( kMesh, kTest, akLines ) )
    						{
    							mask.set( index );
    						}
    					}
    				}
    			}
    		}
    	}
    }
    
    
    /**
     * Test if the input point is inside the mesh.
     * @param origin point to test for inside/outside mesh.
     * @param directions set of randomised directions for counting mesh-intersections (odd = inside, even = outside).
     * @return true when the point is inside the mesh, false otherwise.
     */
    private boolean testIntersections( TriMesh kMesh, Vector3f origin, Line3f[] akLines )
    {
    	int[] lineIntersectionCount = new int[akLines.length]; 
    	
    	
        // Compute intersections with the model-space triangles.
		Triangle3f kTriangle = new Triangle3f();
        int iTQuantity = kMesh.GetTriangleQuantity();
    	IntrLine3Triangle3f kIntr = new IntrLine3Triangle3f();
        
        int iV0, iV1, iV2;
        int[] aiTris = new int[3];
        
        for (int i = 0; i < iTQuantity; i++)
        {
            if (!kMesh.GetTriangle(i,aiTris) )
            {
                continue;
            }

            iV0 = aiTris[0];
            iV1 = aiTris[1];
            iV2 = aiTris[2];

            kMesh.VBuffer.GetPosition3(iV0, kTriangle.V[0]);
            kMesh.VBuffer.GetPosition3(iV1, kTriangle.V[1]);
            kMesh.VBuffer.GetPosition3(iV2, kTriangle.V[2]);
            
            for ( int j = 0; j < akLines.length; j++ )
            {
            	kIntr.Line = akLines[j];
            	kIntr.Triangle = kTriangle;
            	if (kIntr.Find() && 0 <= kIntr.GetLineT() &&  kIntr.GetLineT() <= Float.MAX_VALUE )
            	{
            		lineIntersectionCount[j]++;
            	}
            }  	
        }
        int oddCount = 0;
        for ( int j = 0; j < akLines.length; j++ )
        {
        	if ( (lineIntersectionCount[j]%2) == 1 )
        	{
        		oddCount++;
        	}
        }
    	return ( oddCount >= (1 + akLines.length/2) );
    }
    
    

    public void smoothMesh( TriMesh mesh, int iteration, float alpha, boolean volumeLimit, float volumePercent)
    {
        float initialVolume = 0.0f;
        float presentVolume;
        boolean noVolumeLimit = true;
        float presentPercent;

        HashSet[] connections = buildConnections(mesh);
        
        if (volumeLimit) {
            initialVolume = ComputeVolume(mesh);
        }

        // repeat for however many iterations
        for (int k = 0; (k < iteration) && noVolumeLimit; k++) {
            scaleMesh( mesh, alpha, connections );
            if (volumeLimit) {
                presentVolume = ComputeVolume(mesh);
                presentPercent = Math.abs(100.0f * (presentVolume - initialVolume) / initialVolume);

                if (presentPercent >= volumePercent) {
                    noVolumeLimit = false;
                }
            } // if (doVolumeLimit)
        }

        mesh.UpdateMS();
    }
    

    private HashSet[] buildConnections( TriMesh mesh ) {
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
    
    
    
    

    
    /**
     * Calculates volume of triangle mesh. The mesh consists of triangle faces and encloses a bounded region. Face j, 0
     * <= j <= n-1 has verticies P0, P1, and P2. The order of the verticies is counterclockwise as you view the face
     * from outside the bounded region. The mesh is closed and manifold in the sense that each edge is shared by exactly
     * two triangles. The volume of the bounded region is:<br>
     *
     * <pre>
             V = 1/6 (Sum from j=0 to n-1 of {P0 dot P1 cross P2})
     *   </pre>
     *
     * The terms of the summation can be positive, negative, or zero. The term is positive if the face is
     * counterclockwise when viewed from the zero vector, or zero if the face appears to be a line segment when viewed
     * from the zero vector. NOTICE THAT THERE ARE 2 DIFFERENT DEFINITIONS OF COUNTERCLOCKWISE, COUNTERCLOCKWISE AS
     * VIEWED FROM OUTSIDE THE BOUNDED REGION AND COUNTERCLOCKWISE AS VIEWED FROM THE ZERO VECTOR.
     *
     * <p>A 3D image on a rectangular lattice contains points (i0, i1, i2) where 0 <= ik < Bk for specified dimension
     * bounds Bk. These are just indicies. The actual physical measurements are provided by scaling factors Dk > 0. For
     * example, a 256x256x256 MRI has B0 = B1 = B2 = 256. If each voxel is 1 mm in x, 1 mm in y, and 5 mm in z, then D0
     * = D1 = 1 and D2 = 5. The 3D image encloses a rectangular region [0,C0] x [0,C1] x [0,C2] in <em>physical
     * space</em> where Ck = Dk*Bk. In the example, C0 = D0*B0 = 256 mm in x, C1 = D1*B1 = 256 mm in y, and C2 = D2*B2 =
     * 1280 mm in z. Volume calculations are required to use physical measurements. In the example, volume will be in
     * cubic millimeters.</p>
     *
     * <p>The surface extraction is performed by mapping [0,C0] x [0,C1] x [0,C2] into [-1,1] x [-1,1] x [-1,1] using
     * uniform scaling. This is done to keep the floating point values within order 1 to avoid the floating point errors
     * that occur if you were to use the index values themselves. The topology of a level surface is invariant under any
     * scaling (not just uniform), but the continuous level of detail algorithm for triangle decimation does edge
     * collapses based on various geometric measurements of the mesh representing the level surface. The geometric
     * measurements are not invariant under nonuniform scaling. Map the image into a cube using uniform scaling so that
     * the triangle collapse order is invariant. The uniform scaling is done so that the largest image dimension [0,M]
     * is mapped to [-1,1]. The other ranges are mapped to intervals of the form [-L,L) where L < 1. If (i0,i1,i2) is in
     * [0,B0) x [0,B1) x [0,B2), the corresponding (x0,x1,x2) in [-1,1) is:<br>
     * </p>
     *
     * <pre>
                     2*Dk*ik - Ck
             xk =    ------------
                     max{C0,C1,C2}
     *   </pre>
     *
     * <p>However, we want to map from [0,Bk) to an inclusive interval [-Rk,Rk], where 0 < Rk < 1 and Rk = 1 -
     * Dk/max{C0,C1,C2}. This ensures that surfaces begin in the center of a voxel rather than at the (0,0,0) corner of
     * the voxel. The problem is easiest to see in the Z direction: a surface that should cover the full Z range will
     * end before the last slice. Therefore, the formula should be:<br>
     * </p>
     *
     * <pre>
                     2*Dk*ik - Ck + Dk
             xk =    -----------------
                       max{C0,C1,C2}
     *   </pre>
     *
     * <p>Once a closed manifold triangle mesh is extracted, the problem is now to compute its volume in physical space.
     * Note that ik are indicies in the original image, but the true physical length that is measured (relative to other
     * index locations) is yk = Dk*ik. Any triangle mesh (x0,x1,x2) must be mapped to (y0,y1,y2) before the volume
     * calculation. The mapping is:<br>
     * </p>
     *
     * <pre>
                     max{C0,C1,C2}*xk + Ck - Dk
             yk =    --------------------------
                                  2
     *   </pre>
     *
     * <p>The volume calculations use the previously mentioned formula where the P points are the (y0,y1,y2) values.</p>
     *
     * @return  The volume of the surface.
     */
    public float ComputeVolume( TriMesh mesh )
    {
        float fSum = 0.0f; 
        int iTriangleQuantity = mesh.GetTriangleQuantity();
        int[] aiConnect = mesh.IBuffer.GetData();
        Vector3f kPos0 = new Vector3f();
        Vector3f kPos1 = new Vector3f();
        Vector3f kPos2 = new Vector3f();
        for (int iT = 0; iT < iTriangleQuantity; iT++) {

            // get indices to triangle vertices
            int iV0 = aiConnect[iT * 3 + 0];
            int iV1 = aiConnect[iT * 3 + 1];
            int iV2 = aiConnect[iT * 3 + 2];

            // get vertices
            mesh.VBuffer.GetPosition3(iV0, kPos0);
            mesh.VBuffer.GetPosition3(iV1, kPos1);
            mesh.VBuffer.GetPosition3(iV2, kPos2);
            
            // compute triple scalar product
            // The scalar triple product of three vectors A, B, and C is denoted
            // [A,B,C] and defined by
            // [A,B,C] = A dot ( B x C)
            // = B dot ( C x A)
            // = C dot ( A x B)
            // = det (A (B C) )
            // = | A1 A2 A3 |
            // | B1 B2 B3 |
            // | C1 C2 C3 |
            // V = 1/6 (Sum from j=0 to n-1 of {P0 dot P1 cross P2})
            // P0 = y0, P1 = y1, P2 = y2
            // fProd = P0 dot (P1 x P2)
            // fSum = sum of fProds
            // volume returned = 1/6 fSum
            float fProd = (kPos0.X * ((kPos1.Y * kPos2.Z) - (kPos1.Z * kPos2.Y))) +
                          (kPos0.Y * ((kPos1.Z * kPos2.X) - (kPos1.X * kPos2.Z))) +
                          (kPos0.Z * ((kPos1.X * kPos2.Y) - (kPos1.Y * kPos2.X)));

            fSum += fProd;
        }
    	float[] res = srcImage.getResolutions(0);
        fSum = (Math.abs(fSum / 6.0f) * res[0] * res[1] * res[2]);
        return fSum;
    }

    
    
    
    

	/**
	 * Computes a volume mask of the triangle mesh surface. The BitSet mask volume has the same volume dimensions as the current image.
	 * The mask is used to show the surface-plane intersections in the slice views.
	 * @param mesh triangle mesh to convert to a volume mask representation.
	 * @return BitSet mask, which is set to true wherever the triangle mesh intersects the volume voxel.
	 */
	public BitSet computeSurfaceMask( TriMesh mesh )
	{
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
    
    
    
    private void floodFill(BitSet mask, Vector3f cog)
    {
    	int length = dimX * dimY * dimZ;
    	BitSet visited = new BitSet(length);
    	BitSet whiteMatter = new BitSet(length);
    	
    	Vector<Vector3f> seedList = new Vector<Vector3f>();
    	seedList.add(cog);
		int index = (int) (cog.Z * dimX * dimY + cog.Y * dimX + cog.X);	
		visited.set(index);
		whiteMatter.set(index);
		
    	
		fill( seedList, visited, mask );

		visited.clear();
		visited = null;
		whiteMatter.clear();
		whiteMatter = null;
    	
    }
    private void fill( Vector<Vector3f> seedList, BitSet visited, BitSet mask )
    {    	
    	
    	while ( seedList.size() > 0 )
    	{
    		Vector3f seed = seedList.remove(0);

    		int x = (int) seed.X;
    		int y = (int) seed.Y;
    		int z = (int) (seed.Z -1);
    		if ( inBounds( x, y, z ) )
    		{
    			int index = (int) (z * dimX * dimY + y * dimX + x);
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
    		if ( inBounds( x, y, z ) )
    		{
    			int index = (int) (z * dimX * dimY + y * dimX + x);
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
    		if ( inBounds( x, y, z ) )
    		{
    			int index = (int) (z * dimX * dimY + y * dimX + x);
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
    		if ( inBounds( x, y, z ) )
    		{
    			int index = (int) (z * dimX * dimY + y * dimX + x);
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
    		if ( inBounds( x, y, z ) )
    		{
    			int index = (int) (z * dimX * dimY + y * dimX + x);
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
    		if ( inBounds( x, y, z ) )
    		{
    			int index = (int) (z * dimX * dimY + y * dimX + x);
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
    
    private void subtractMask( ModelImage image )
    {

    	for ( int z = 0; z < dimZ; z++ )
    	{
    		for ( int y = 0; y < dimY; y++ )
    		{
    			for ( int x = 0; x < dimX; x++ )
    			{
					int index = z * dimX * dimY + y * dimX + x;
					if ( image.getMask().get(index) )
					{
						image.set( index, min_2P );
					}
    			}
    		}
    	}
    }
    
    
    private BitSet outlineMask( BitSet mask )
    {
    	int index;
    	int length = dimX * dimY * dimZ;
    	BitSet newMask = new BitSet(length);

    	for ( int z = 0; z < dimZ; z++ )
    	{
    		for ( int y = 0; y < dimY; y++ )
    		{
    			for ( int x = 0; x < dimX; x++ )
    			{
    	    		index = z * dimX*dimY + y * dimX + x;
    	    		if ( mask.get(index) )
    	    		{
    	    			break;
    	    		}
    	    		newMask.set(index);
    			}
    			for ( int x = dimX-1; x >= 0; x-- )
    			{
    	    		index = z * dimX*dimY + y * dimX + x;
    	    		if ( mask.get(index) )
    	    		{
    	    			break;
    	    		}
    	    		newMask.set(index);
    			}
    		}
    	}
 	
    	for ( int z = 0; z < dimZ; z++ )
    	{
			for ( int x = 0; x < dimX; x++ )
			{
    			for ( int y = 0; y < dimY; y++ )
    			{
    	    		index = z * dimX*dimY + y * dimX + x;
    	    		if ( mask.get(index) )
    	    		{
    	    			break;
    	    		}
    	    		newMask.set(index);
    			}
    			for ( int y = dimY-1; y >= 0; y-- )
    			{
    	    		index = z * dimX*dimY + y * dimX + x;
    	    		if ( mask.get(index) )
    	    		{
    	    			break;
    	    		}
    	    		newMask.set(index);
    			}
    		}
    	}

//		for ( int x = 0; x < dimX; x++ )
//    	{
//    		for ( int y = 0; y < dimY; y++ )
//    		{
//    			for ( int z = 0; z < dimZ; z++ )
//    			{
//    	    		index = z * dimX*dimY + y * dimX + x;
//    	    		if ( mask.get(index) )
//    	    		{
//    	    			break;
//    	    		}
//    	    		newMask.set(index);
//    			}
//    			for ( int z = dimZ-1; z >= 0; z-- )
//    			{
//    	    		index = z * dimX*dimY + y * dimX + x;
//    	    		if ( mask.get(index) )
//    	    		{
//    	    			break;
//    	    		}
//    	    		newMask.set(index);
//    			}
//    		}
//    	}
    
    	
    	return newMask;
    }
    
    
    private TriMesh createMesh( BitSet mask )
    {
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
    	if ( kMesh == null )
    	{
    		System.err.println( "surface extration failed" );
    	}
    	if ( kMesh != null )
    	{
    		// Get the adjacent triangles:
    		VETMesh kVETMesh = new VETMesh(2 * kMesh.VBuffer.GetVertexQuantity(), .9f, 2 * kMesh.IBuffer
    				.GetIndexQuantity(), .9f, 2 * kMesh.GetTriangleQuantity(), .9f, kMesh.IBuffer.GetData());
    		kMesh.IBuffer = new IndexBuffer(kVETMesh.GetTriangles());
    	}
    	return kMesh;

    }

    private TriMesh convexHull( TriMesh mesh )
    {
		ConvexHull3f convexHull = new ConvexHull3f( mesh.VBuffer.GetVertexQuantity(), mesh.VBuffer.GetPositionArray(), 0.00001f, true );
		IndexBuffer iBuffer = new IndexBuffer(convexHull.GetIndices());
		VertexBuffer vBuffer = new VertexBuffer( mesh.VBuffer );
		TriMesh convexHullMesh = new TriMesh( vBuffer, iBuffer );

//		System.err.println( convexHullMesh.VBuffer.GetVertexQuantity() );
		convexHullMesh = removeUnusedVertices(convexHullMesh);
		return convexHullMesh;
    }
    

	private ModelImage randomize( ModelImage image, int stepSize )
	{
		ModelImage blurImage = new ModelImage(srcImage.getType(), srcImage.getExtents(),
				JDialogBase.makeImageName(srcImage.getImageName(), "_blur"));
		int stepsX = dimX / stepSize;
		int stepsY = dimY / stepSize;
		int stepsZ = dimZ / stepSize;
		int length = stepsX*stepsY*stepsZ;
		Vector2d[] randomArray = new Vector2d[length];		
		Random gen = new Random();
		for ( int i = 0; i < length; i++ )
		{
			randomArray[i] = new Vector2d( gen.nextInt(), i );
		}
    	Arrays.sort( randomArray );

		for ( int z = 0; z < stepsZ; z++ )
		{
			for ( int y = 0; y < stepsY; y++ )
			{
				for ( int x = 0; x < stepsX; x++ )
				{								
					// index into the random array:
					int randomIndex = z * stepsY * stepsX + y * stepsX + x;	
					
					// recreate randomized index into random array:
					int targetIndex = (int) randomArray[randomIndex].Y;					
					int targetX = targetIndex % stepsX;
					targetIndex -= targetX;
					targetIndex /= stepsX;
					
					int targetY = targetIndex % stepsY;
					targetIndex -= targetY;
					targetIndex /= stepsY;
					
					int targetZ = targetIndex;
					
					// go from random array to big array:					
					targetZ *= stepSize;
					targetY *= stepSize;
					targetX *= stepSize;
					
					int srcZ = z * stepSize;
					for ( int z1 = targetZ; z1 < targetZ + stepSize; z1++ )
					{					
						int srcY = y * stepSize;
						for ( int y1 = targetY; y1 < targetY + stepSize; y1++ )
						{	
							int srcX = x * stepSize;
							for ( int x1 = targetX; x1 < targetX + stepSize; x1++ )
							{
//								System.err.println( x1 + " " + y1 + " " + z1 + "      " + srcX + " " + srcY + " " + srcZ );
								blurImage.set( x1,y1,z1, image.getFloat(srcX++, srcY, srcZ) );
							}							
							srcY++;
						}						
						srcZ++;
					}
				}
			}
		}
		
		blurImage.calcMinMax();
		return blurImage;
	}
	
	private Vector3f getPlaneNormal( int faceOrientation )
	{
		float temp = (float) (1f/Math.sqrt(2f));
        if(faceOrientation == JDialogFaceAnonymize.FACING_LEFT) 
        {
        	return new Vector3f( temp, -temp, 0 );
        }
        else if(faceOrientation == JDialogFaceAnonymize.FACING_RIGHT) 
        {
        	return new Vector3f( -temp, -temp, 0 );
        }
        else if(faceOrientation == JDialogFaceAnonymize.FACING_DOWN) 
        {
        	return new Vector3f( 0, -temp, -temp );
        }
        else if(faceOrientation == JDialogFaceAnonymize.FACING_UP) 
        {
        	return new Vector3f( 0, temp, temp );
        }
        else if(faceOrientation == JDialogFaceAnonymize.FACING_INTO_SCREEN) 
        {
        	return new Vector3f( 0, -temp, -temp );
        }
        else if(faceOrientation == JDialogFaceAnonymize.FACING_OUT_OF_SCREEN) 
        {
        	return new Vector3f( 0, -temp, temp );
        }
    	return new Vector3f( temp, -temp, 0 );
	}
	
	private Vector3f getPlaneCenter( int faceOrientation )
	{
        if(faceOrientation == JDialogFaceAnonymize.FACING_LEFT) {
        	return new Vector3f( 0, dimY, dimZ / 2 );
        }
        else if(faceOrientation == JDialogFaceAnonymize.FACING_RIGHT) {
        	return new Vector3f( dimX, dimY, dimZ / 2 );
        }
        else if(faceOrientation == JDialogFaceAnonymize.FACING_DOWN) {
        	return new Vector3f( dimX/2, dimY, dimZ );
        }
        else if(faceOrientation == JDialogFaceAnonymize.FACING_UP) {
        	return new Vector3f( dimX/2, 0, 0 );
        }
        else if(faceOrientation == JDialogFaceAnonymize.FACING_INTO_SCREEN) {
        	return new Vector3f( dimX/2, dimY, dimZ );
        }
        else if(faceOrientation == JDialogFaceAnonymize.FACING_OUT_OF_SCREEN) {
        	return new Vector3f( dimX/2, dimY, 0 );
        }
    	return new Vector3f( 0, dimY, dimZ / 2 );
		
	}

	private BitSet createMask( Plane3f clipPlane )
	{
		BitSet mask = new BitSet(dimX*dimY*dimZ);
		Vector3f pt = new Vector3f();
		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					pt.set(x,y,z);
					float distance = clipPlane.DistanceTo( pt );
					if ( distance < 0 )
					{
						mask.set( z*dimY*dimX + y*dimX + x);
					}
				}
			}
		}
		return mask;
	}
	
	private BitSet createMask( TriMesh mesh, boolean show, String postScript )
	{
		BitSet meshMask = computeSurfaceMask(mesh);	
		floodFill(meshMask, cog);
		if ( show )
		{
			ModelImage maskImage = (ModelImage)destImage.clone();
			maskImage.setMask(meshMask);
			maskImage.setImageName( destImage.getImageName() + postScript );        	
			new ViewJFrameImage(maskImage);		
		}
		return meshMask;
	}

}
