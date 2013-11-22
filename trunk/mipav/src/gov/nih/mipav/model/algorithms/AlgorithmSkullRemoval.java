package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Intersection.IntrLine3Triangle3f;
import WildMagic.LibFoundation.Intersection.IntrSegment3Triangle3f;
import WildMagic.LibFoundation.Mathematics.Line3f;
import WildMagic.LibFoundation.Mathematics.Plane3f;
import WildMagic.LibFoundation.Mathematics.Segment3f;
import WildMagic.LibFoundation.Mathematics.Triangle3f;
import WildMagic.LibFoundation.Mathematics.Vector2d;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Meshes.ConvexHull3f;
import WildMagic.LibFoundation.Meshes.VETMesh;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.SceneGraph.BoxBV;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR3D;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRotate;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;

import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogFaceAnonymize;
import gov.nih.mipav.view.renderer.WildMagic.Interface.FileSurface_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.SurfaceExtractorCubes;

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
	private int dimX;
	private int dimY;
	private int dimZ;

	private boolean blur = false;
	private boolean remove = false;
	private boolean face = false;
	private boolean showFaceSegmentation = false;
	private boolean showSkullSegmentation = false;
	private float offSet = 0;
	
	private int[] originalAxis;
	private int[] targetAxis;
	
	private int[] originalAxisTargetAtlas;
	private boolean reMappedSrc = false;
	private boolean reMappedTargetAtlas = false;
	
	private ModelImage atlasBasedImage;
	private ModelImage registeredSourceImage;
	
    /**
     * Construct the face anonymizer, but do not run it yet.
     *
     * @param  srcImg            The image to de-face
     * @param  faceDirection     the orientation of the patient's face, as determined by the dialog
     */
    public AlgorithmSkullRemoval(ModelImage srcImg, int faceDirection)
    {
    	super( null, srcImg );

    	targetAxis = new int[]{FileInfoBase.ORI_A2P_TYPE, FileInfoBase.ORI_S2I_TYPE,
                FileInfoBase.ORI_L2R_TYPE};
    	originalAxis = srcImage.getAxisOrientation();
        int[] axisOrder = { 0, 1, 2, 3 };
        boolean[] axisFlip = { false, false, false, false };
        if ( reMappedSrc = MipavCoordinateSystems.matchOrientation( targetAxis, originalAxis, axisOrder, axisFlip ) )
        {
            AlgorithmRotate rotateAlgo = new AlgorithmRotate( srcImage, axisOrder, axisFlip );
            rotateAlgo.setRunningInSeparateThread(false);
            rotateAlgo.run();
            srcImage = rotateAlgo.returnImage();
        }
        this.faceOrientation = JDialogFaceAnonymize.FACING_LEFT;
        
        
    	destImage = (ModelImage) srcImage.clone();
    	destImage.setImageName( srcImage.getImageName() + "_deskull" );
        faceOrientation = faceDirection;
        
    	dimX = srcImage.getExtents().length > 0 ? srcImage.getExtents()[0] : 1;
    	dimY = srcImage.getExtents().length > 1 ? srcImage.getExtents()[1] : 1;
    	dimZ = srcImage.getExtents().length > 2 ? srcImage.getExtents()[2] : 1;    	
    }
    
    /**
     * Construct the face anonymizer, but do not run it yet.
     *
     * @param  srcImg            The image to de-face
     * @param  faceDirection     the orientation of the patient's face, as determined by the dialog
     */
    public AlgorithmSkullRemoval(ModelImage srcImg, ModelImage atlasImage, int faceDirection)
    {
    	super( null, atlasImage );

    	targetAxis = new int[]{FileInfoBase.ORI_A2P_TYPE, FileInfoBase.ORI_S2I_TYPE,
                FileInfoBase.ORI_L2R_TYPE};
    	originalAxis = srcImage.getAxisOrientation();
        int[] axisOrder = { 0, 1, 2, 3 };
        boolean[] axisFlip = { false, false, false, false };
        if ( reMappedSrc = MipavCoordinateSystems.matchOrientation( targetAxis, originalAxis, axisOrder, axisFlip ) )
        {
            AlgorithmRotate rotateAlgo = new AlgorithmRotate( srcImage, axisOrder, axisFlip );
            rotateAlgo.setRunningInSeparateThread(false);
            rotateAlgo.run();
            srcImage = rotateAlgo.returnImage();
        }
        

    	this.atlasBasedImage = srcImg;
    	originalAxisTargetAtlas = srcImg.getAxisOrientation();
        axisOrder = new int[]{ 0, 1, 2, 3 };
        axisFlip = new boolean[]{ false, false, false, false };
        if ( reMappedTargetAtlas = MipavCoordinateSystems.matchOrientation( targetAxis, originalAxisTargetAtlas, axisOrder, axisFlip ) )
        {
        	AlgorithmRotate rotateAlgo = new AlgorithmRotate( srcImg, axisOrder, axisFlip );
        	rotateAlgo.setRunningInSeparateThread(false);
        	rotateAlgo.run();
        	this.atlasBasedImage = rotateAlgo.returnImage();
        }
        this.faceOrientation = JDialogFaceAnonymize.FACING_LEFT;

        registeredSourceImage = registerImages( atlasBasedImage, srcImage );
        if ( registeredSourceImage != null )
        {
        	if ( reMappedSrc )
        	{
        		srcImage.disposeLocal();
        	}
        	srcImage = registeredSourceImage;
//        	new ViewJFrameImage(srcImage);
        }
        
    	destImage = (ModelImage) srcImage.clone();
    	destImage.setImageName( srcImage.getImageName() + "_deskull" );
        faceOrientation = faceDirection;
        
    	dimX = srcImage.getExtents().length > 0 ? srcImage.getExtents()[0] : 1;
    	dimY = srcImage.getExtents().length > 1 ? srcImage.getExtents()[1] : 1;
    	dimZ = srcImage.getExtents().length > 2 ? srcImage.getExtents()[2] : 1;
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
        

        fireProgressStateChanged(100);
        
        setCompleted(true);	
    }
    
    public void setOutputOption( boolean blur, boolean remove, boolean face, boolean showFaceSegmentation, boolean showSkullSegmentation )
    {
    	this.blur = blur;
    	this.remove = remove;
    	this.face = face;
    	this.showFaceSegmentation = showFaceSegmentation;
    	this.showSkullSegmentation = showSkullSegmentation;
    }
    
    public void setOffSet( float offSet )
    {
    	this.offSet = offSet;
    }

    
    private void skullRemoval()
    {
        fireProgressStateChanged(0, null, "Estimating parameters ...");

        estimateParameters();
        
        fireProgressStateChanged(30);
        
        useCSFMin = true;
        VOI seedPoints;
    	seedPoints = estimateWhiteMatter(destImage); 
    	

    	boolean noMax = true;
//    	for ( int i = 0; i < seedPoints.getCurves().size(); i++ )
//    	{
//    		VOIBase seedVOI = seedPoints.getCurves().elementAt(i);
//    		for ( int j = 0; j < seedVOI.size(); j++ )
//    		{
//    			Vector3f seed = seedVOI.elementAt(j);    		
//        		int index = (int) (seed.Z * dimX * dimY + seed.Y * dimX + seed.X);	
//        		float value = destImage.getFloat(index);
//        		if ( value >= csfThresholdMax )
//        		{
//        			noMax = false;
//        		}
//    		}
//    	}
//    	if ( noMax )
    	{
    		gmThresholdMax = max_98P;
    		Preferences.debug( "White matter thresholds : " + gmThresholdMin + " " + gmThresholdMax + " " + noMax + "\n", Preferences.DEBUG_ALGORITHM );
    		System.err.println( "White matter thresholds : " + gmThresholdMin + " " + gmThresholdMax + " " + noMax + "\n" );
    	}
//    	else
//    	{
//    		gmThresholdMin = min_2P;
//    		Preferences.debug( "White matter thresholds : " + gmThresholdMin + " " + gmThresholdMax + " " + noMax + "\n", Preferences.DEBUG_ALGORITHM );
//    		System.err.println( "White matter thresholds : " + gmThresholdMin + " " + gmThresholdMax + " " + noMax + "\n" );
//    	}
        fillWhiteMatter(destImage, seedPoints);
        
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
        
        if ( reMappedSrc )
        {
        	int[] axisOrder = { 0, 1, 2, 3 };
        	boolean[] axisFlip = { false, false, false, false };
        	if ( MipavCoordinateSystems.matchOrientation( originalAxis, targetAxis, axisOrder, axisFlip ) )
        	{
        		AlgorithmRotate rotateAlgo = new AlgorithmRotate( destImage, axisOrder, axisFlip );
        		rotateAlgo.setRunningInSeparateThread(false);
        		rotateAlgo.run();
        		destImage.disposeLocal();
        		destImage = rotateAlgo.returnImage();
        	}
        }

    	if ( atlasBasedImage != null )
    	{
        	int[] axisOrder = { 0, 1, 2, 3 };
        	boolean[] axisFlip = { false, false, false, false };
        	if ( MipavCoordinateSystems.matchOrientation( atlasBasedImage.getAxisOrientation(), destImage.getAxisOrientation(),  
        			axisOrder, axisFlip ) )
        	{
        		AlgorithmRotate rotateAlgo = new AlgorithmRotate( destImage, axisOrder, axisFlip );
        		rotateAlgo.setRunningInSeparateThread(false);
        		rotateAlgo.run();
        		destImage.disposeLocal();
        		destImage = rotateAlgo.returnImage();
        	}
            if ( reMappedTargetAtlas )
            {
        		atlasBasedImage.setMask( destImage.getMask() );
            	if ( MipavCoordinateSystems.matchOrientation( originalAxisTargetAtlas, targetAxis, axisOrder, axisFlip ) )
            	{
            		AlgorithmRotate rotateAlgo = new AlgorithmRotate( atlasBasedImage, axisOrder, axisFlip );
            		rotateAlgo.setRunningInSeparateThread(false);
            		rotateAlgo.run();
            		atlasBasedImage.disposeLocal();
            		atlasBasedImage = rotateAlgo.returnImage();
            	}
        		destImage.disposeLocal();
                destImage = atlasBasedImage;
            }
            else
            {
            	ModelImage temp = (ModelImage)atlasBasedImage.clone();
            	temp.setMask( destImage.getMask() );
            	destImage.disposeLocal();
            	destImage = temp;
            }
//        	new ViewJFrameImage(atlasBasedImage);        	
    	}


    	if ( remove )
    	{
    		subtractMask( destImage );
    		destImage.setImageName( srcImage.getImageName() + "_deskull" );
    	}

    	if ( blur )
    	{
    		ModelImage pixelizedImage = randomize( destImage, 10 );	        	            
    		addBlur( pixelizedImage, destImage );
    		pixelizedImage.disposeLocal();
    		pixelizedImage = null;
    		destImage.setImageName( srcImage.getImageName() + "_deskull_blur" );
    	}
    	
    	
    	if ( showFaceSegmentation | showSkullSegmentation )
    	{
    		destImage.resetVOIs();
    		
        	short id = (short) destImage.getVOIs().getUniqueID();
        	VOI centerGravity = new VOI(id, "centerGravity", VOI.POINT, 1f );
        	centerGravity.importPoint(cog);
        	destImage.registerVOI(centerGravity);
        	centerGravity.setColor( Color.green );    	
        	destImage.setImageName( srcImage.getImageName() + "_outsideMask" );
    	}
    	else
    	{    		        	
    		destImage.resetVOIs();
    		destImage.clearMask();
    		destImage.calcMinMax();
    	}
        fireProgressStateChanged(100);
		new ViewJFrameImage( destImage );

		if ( reMappedSrc || (registeredSourceImage != null) )
		{
			srcImage.disposeLocal();
			srcImage = null;
		}
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
		TriMesh whiteMatterMesh = createMesh( image.getMask() );
		if ( whiteMatterMesh != null )
		{
			TriMesh convexHullMesh = convexHull(whiteMatterMesh);

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
					if ( offSet != 0 )
					{
						scaleMesh(convexHullBrain);
					}
					destImage.setMask( createMask( convexHullBrain, false, "ch_bet_mask" ) );
					kMesh = convexHullBrain;
				}
				
				if ( face || showFaceSegmentation )
				{
					Vector3f front = getFront( faceOrientation, cog );
					Vector3f bottom = getBottom( faceOrientation, cog );
//					System.err.println( "COG " + cog );
//					System.err.println( "Front " + front );
//					System.err.println( "Bottom " + bottom );
					Segment3f frontSegment = new Segment3f( front, cog );
					Segment3f bottomSegment = new Segment3f( bottom, cog );
					IntrSegment3Triangle3f intersection = new IntrSegment3Triangle3f();
					
					int[] aiTris = new int[3];
					int iTQuantity = kMesh.GetTriangleQuantity();
					int iV0, iV1, iV2;
					Vector3f kV0 = new Vector3f();
					Vector3f kV1 = new Vector3f();
					Vector3f kV2 = new Vector3f();

					float minDistFront = Float.MAX_VALUE;
					Vector3f minDistFrontPoint = new Vector3f();
					float minDistBottom = Float.MAX_VALUE;
					Vector3f minDistBottomPoint = new Vector3f();
					for ( int i = 0; i < iTQuantity; i++ )
					{
			            if (!kMesh.GetTriangle(i, aiTris) )
			            {
			                continue;
			            }
						iV0 = aiTris[0];
						iV1 = aiTris[1];
						iV2 = aiTris[2];

						kMesh.VBuffer.GetPosition3(iV0, kV0);
						kMesh.VBuffer.GetPosition3(iV1, kV1);
						kMesh.VBuffer.GetPosition3(iV2, kV2);
						
						Triangle3f tri = new Triangle3f(kV0, kV1, kV2);
						intersection.Triangle = tri;
						
						intersection.Segment = frontSegment;
				    	if ( intersection.Find() )
				    	{
				    		// add intersection point:
				    		Vector3f intersectionPoint = new Vector3f();
				    		intersectionPoint.scaleAdd( intersection.GetSegmentParameter(), frontSegment.Direction, frontSegment.Center);
				    		
				    		float distance = front.distance(intersectionPoint);
				    		if ( distance < minDistFront )
				    		{
				    			minDistFront = distance;
				    			minDistFrontPoint.copy(intersectionPoint);
				    		}
				    	}
						
						intersection.Segment = bottomSegment;
				    	if ( intersection.Find() )
				    	{
				    		// add intersection point:
				    		Vector3f intersectionPoint = new Vector3f();
				    		intersectionPoint.scaleAdd( intersection.GetSegmentParameter(), bottomSegment.Direction, bottomSegment.Center);
				    		
				    		float distance = bottom.distance(intersectionPoint);
				    		if ( distance < minDistBottom )
				    		{
				    			minDistBottom = distance;
				    			minDistBottomPoint.copy(intersectionPoint);
				    		}
				    	}
					}
//					System.err.println( minDistFrontPoint );
//					System.err.println( minDistBottomPoint );
					
					Plane3f clipPlane = new Plane3f( new Vector3f( minDistFrontPoint.X, minDistFrontPoint.Y, 0),
							new Vector3f( minDistBottomPoint.X, minDistBottomPoint.Y, 0) , 
							new Vector3f( minDistFrontPoint.X, minDistFrontPoint.Y, dimZ));

//					System.err.println( "Normal " + clipPlane.Normal );
					
					
//					Plane3f clipPlane = new Plane3f( getPlaneNormal( faceOrientation ), getPlaneCenter( faceOrientation ) );
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
//						if ( distance < 0 )
//						{
//							System.err.println( "Plane error " + kMesh.VBuffer.GetPosition3(i) );
//						}
					}
//					System.err.println( minIndex  + " " + minDist );
					if ( minIndex != -1 )
					{
						Vector3f pos = kMesh.VBuffer.GetPosition3(minIndex);						
						clipPlane = new Plane3f( clipPlane.Normal, pos );
						BitSet clipMask = createMask( clipPlane, faceOrientation, cog );
//						if ( showSegmentation )
//						{
//							ModelImage outlineMaskImage = (ModelImage)destImage.clone();
//							outlineMaskImage.setMask(clipMask);
//							outlineMaskImage.setImageName( destImage.getImageName() + "_ClipMask" );  
//							outlineMaskImage.resetVOIs();      	
//							new ViewJFrameImage(outlineMaskImage);
//						}
						if ( face || showFaceSegmentation )
						{
							destImage.setMask(clipMask);
						}
					}
				}
			}
		}
		if ( !(face || showFaceSegmentation) )
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
		int dimX = destImage.getExtents()[0];
		int dimY = destImage.getExtents()[1];
		int dimZ = destImage.getExtents()[2];
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
    
    private void fillWhiteMatter( ModelImage image, VOI seedPoints )
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

    	Vector<Vector3f> originalSeedList = new Vector<Vector3f>();
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
    			originalSeedList.add(seed);
    		}
    	}    	
    	
		fill( image, originalSeedList, seedList, visited, whiteMatter );
    	
		image.setMask( whiteMatter );
    }
    
    private void fill( ModelImage image, Vector<Vector3f> originalSeedList, Vector<Vector3f> seedList, BitSet visited, BitSet whiteMatter )
    {
    	Vector3f min = new Vector3f(dimX, dimY, dimZ);
    	Vector3f max = new Vector3f(0,0,0);
		getBoundsDistance( min, max, originalSeedList, 120 );
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
								float value = image.getFloat(x, y, z);
								if ( (value <= gmThresholdMin) || (value >= gmThresholdMax) )
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
									if ( (x >= min.X) && (x <= max.X) && (y >= min.Y) && (y <= max.Y) && (z >= min.Z) && (z <= max.Z) )
									{
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
    }
    
    
    private void getBoundsDistance( Vector3f min, Vector3f max, Vector<Vector3f> originalSeedList, float limit )
    {
        float xRes = srcImage.getFileInfo(0).getResolutions()[0];
        float yRes = srcImage.getFileInfo(0).getResolutions()[1];
        float zRes = srcImage.getFileInfo(0).getResolutions()[2];
        Vector3f range = new Vector3f( limit/xRes, limit/yRes, limit/zRes );
    	Vector3f pos = new Vector3f();
    	for ( int i = 0; i < originalSeedList.size(); i++ )
    	{
    		pos.copy(originalSeedList.elementAt(i) );
    		min.min( Vector3f.sub( pos, range ) );
    		max.max( Vector3f.add( pos, range ) );
    	}
    	System.err.println( "Bounds = " + min + "   " + max );
    }
    
    
    private VOI estimateWhiteMatter( ModelImage image )
    {
    	Vector<Vector3f> seedList = new Vector<Vector3f>();
    	seedList.add(cog);
    	Vector3f min = new Vector3f(dimX, dimY, dimZ);
    	Vector3f max = new Vector3f(0,0,0);
		getBoundsDistance( min, max, seedList, 60 );
		
    	int cubeSize = 5;
    	int cubeHalf = cubeSize/2;
    	
    	Vector<Vector2d> cubeIntensities = new Vector<Vector2d>();
    	Vector<Vector3f> cubeCenters = new Vector<Vector3f>();
    	for ( int z = (int) min.Z; z < max.Z - cubeSize; z++ )
    	{
    		for ( int y = (int) min.Y; y < max.Y - cubeSize; y++ )
    		{
    			for ( int x = (int) min.X; x < max.X - cubeSize; x++ )
    			{
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
            					}
            				}
        				}    					
    				}
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
    	
    	Vector2d[] sortedIntensities = new Vector2d[cubeIntensities.size()];
    	for ( int i = 0; i < cubeIntensities.size(); i++ )
    	{
    		sortedIntensities[i] = cubeIntensities.elementAt(i);
    	}
    	Arrays.sort( sortedIntensities );

    	System.err.println( "csfMin = " + csfThresholdMin + " " + " max = " + csfThresholdMax + " useMin = " + useCSFMin );
    	System.err.println( "Intensity range =    " + sortedIntensities[0].X + "    " +  sortedIntensities[cubeIntensities.size()-1].X );
    	System.err.println( "Intensity range = " + (sortedIntensities[0].X - sortedIntensities[cubeIntensities.size()-1].X) );
    	System.err.println( "Variance range =    " + sortedIntensities[0].Y + "    " + sortedIntensities[cubeIntensities.size()-1].Y );
    	System.err.println( "Variance range = " + (sortedIntensities[0].Y - sortedIntensities[cubeIntensities.size()-1].Y) );
    	
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
    	double maxIntensity = -Double.MAX_VALUE;
    	
    	for ( int i = 0; i < sortedIntensities.length; i++ )
    	{    		
    		if ( sortedIntensities[i].Y <= minVar )
    		{
    			if ( sortedIntensities[i].X > maxIntensity )
    			{
    				maxIntensity = sortedIntensities[i].X;
    			}
    		}
    	}
    	
    	System.err.println( maxIntensity + " " + minVar );
    	
    	

    	short id = (short) image.getVOIs().getUniqueID();
    	VOI seedPoints = new VOI(id, "SeedPoints", VOI.POINT, 1f );
    	

    	for ( int i = 0; i < cubeCenters.size(); i++ )
    	{
    		if ( cubeIntensities.elementAt(i).X == maxIntensity && cubeIntensities.elementAt(i).Y <= minVar )
    		{	
    			System.err.println( "Seed point: " + cubeCenters.elementAt(i) + "       intensity = " + cubeIntensities.elementAt(i).X + " variance = " + cubeIntensities.elementAt(i).Y );
    			seedPoints.importPoint(cubeCenters.elementAt(i));
    		}
    		
    		
//    		if (cubeIntensities.elementAt(i).Y <= sortedIntensities[minIndex].Y)
//    		{
//    			System.err.println( "Seed point: " + cubeCenters.elementAt(i) + "       intensity = " + cubeIntensities.elementAt(i).X + " variance = " + cubeIntensities.elementAt(i).Y );
//    		}
//    		if ( useCSFMin && (cubeIntensities.elementAt(i).X > csfThresholdMin) && 
//    				(cubeIntensities.elementAt(i).Y == sortedIntensities[minIndex].Y) )
//    		{   			
//    			seedPoints.importPoint(cubeCenters.elementAt(i));
//
//    			Preferences.debug( "Seed point: " + cubeCenters.elementAt(i) + "       intensity = " + cubeIntensities.elementAt(i).X + " variance = " + cubeIntensities.elementAt(i).Y + "\n", Preferences.DEBUG_ALGORITHM );
////    			System.err.println( "Seed point: " + cubeCenters.elementAt(i) + "       intensity = " + cubeIntensities.elementAt(i).X + " variance = " + cubeIntensities.elementAt(i).Y );
//    		}
//    		else if ( !useCSFMin && (cubeIntensities.elementAt(i).X < csfThresholdMax) && 
//    				(cubeIntensities.elementAt(i).Y == sortedIntensities[minIndex].Y) )
//    		{   			
//    			seedPoints.importPoint(cubeCenters.elementAt(i));
//
//    			Preferences.debug( "Seed point: " + cubeCenters.elementAt(i) + "       intensity = " + cubeIntensities.elementAt(i).X + " variance = " + cubeIntensities.elementAt(i).Y + "\n", Preferences.DEBUG_ALGORITHM );
////    			System.err.println( "Seed point: " + cubeCenters.elementAt(i) + "       intensity = " + cubeIntensities.elementAt(i).X + " variance = " + cubeIntensities.elementAt(i).Y );
//    		}
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
    }


	/**
	 * Computes a volume mask of the triangle mesh surface. The BitSet mask volume has the same volume dimensions as the current image.
	 * The mask is used to show the surface-plane intersections in the slice views.
	 * @param mesh triangle mesh to convert to a volume mask representation.
	 * @return BitSet mask, which is set to true wherever the triangle mesh intersects the volume voxel.
	 */
	private BitSet computeSurfaceMask( TriMesh mesh )
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
		ModelImage blurImage = new ModelImage(image.getType(), image.getExtents(),
				JDialogBase.makeImageName(image.getImageName(), "_blur"));
		int stepsX = image.getExtents()[0] / stepSize;
		int stepsY = image.getExtents()[1] / stepSize;
		int stepsZ = image.getExtents()[2] / stepSize;
		int length = stepsX*stepsY*stepsZ;

		float[] blockAvg = new float[length];
		// calculate block average intensities:
		for ( int z = 0; z < stepsZ; z++ )
		{
			for ( int y = 0; y < stepsY; y++ )
			{
				for ( int x = 0; x < stepsX; x++ )
				{													
					int count = 0;
					float sum = 0;
					int srcZ = z * stepSize;
					for ( int z1 = z; z1 < z + stepSize; z1++ )
					{					
						int srcY = y * stepSize;
						for ( int y1 = y; y1 < y + stepSize; y1++ )
						{	
							int srcX = x * stepSize;
							for ( int x1 = x; x1 < x + stepSize; x1++ )
							{
								sum += image.getFloat(srcX++, srcY, srcZ);
								count++;
							}							
							srcY++;
						}						
						srcZ++;
					}
					sum /= (float)(count);
					
					int blockIndex = z * stepsY * stepsX + y * stepsX + x;	
					blockAvg[blockIndex] = sum;
				}
			}
		}
		
		// randomly swap blocks -- choose blocks that are with in 25% of each other:
		boolean[] swapped = new boolean[length];
		int[] blockIndex = new int[length];
		for ( int i = 0; i < length; i++ )
		{
			swapped[i] = false;				
			blockIndex[i] = i;
		}
		for ( int i = 0; i < length; i++ )
		{
			if ( swapped[i] )
				continue;
			for ( int j = length-1; j > i; j-- )
			{
				if ( !swapped[j] && (Math.abs(blockAvg[i] - blockAvg[j]) < .25*blockAvg[i]) )
				{
					int temp = blockIndex[i];
					blockIndex[i] = blockIndex[j];
					blockIndex[j] = temp;
					
					swapped[i] = true;
					swapped[j] = true;
				}
			}
		}
//		for ( int i = 0; i < length; i++ )
//		{
//			System.err.println( blockIndex[i] + "   " + swapped[i] );
//		}
		
		
		
		// Swap the blocks of voxels in the volumes:
		for ( int z = 0; z < stepsZ; z++ )
		{
			for ( int y = 0; y < stepsY; y++ )
			{
				for ( int x = 0; x < stepsX; x++ )
				{								
					// index into the random array:
					int randomIndex = z * stepsY * stepsX + y * stepsX + x;	
					
					// recreate randomized index into random array:
					int targetIndex = blockIndex[randomIndex];					
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
//		new ViewJFrameImage(blurImage);
		return blurImage;
	}

	private void getCogBounds( int faceOrientation, Vector3f cog, Vector3f min, Vector3f max )
	{
		min.set( 0, cog.Y, 0 );
		max.set( cog.X, dimY, dimZ );
	}
	
	
	private Vector3f getFront( int faceOrientation, Vector3f cog )
	{
    	return new Vector3f( 0, cog.Y, cog.Z );
	}
	
	private Vector3f getBottom( int faceOrientation, Vector3f cog )
	{
    	return new Vector3f( cog.X, dimY, cog.Z );
	}
	

	private BitSet createMask( Plane3f clipPlane, int faceOrientation, Vector3f cog )
	{
		Vector3f min = new Vector3f(0,0,0);
		Vector3f max = new Vector3f(dimX,dimY,dimZ);
		getCogBounds( faceOrientation, cog, min, max );
		BitSet mask = new BitSet(dimX*dimY*dimZ);
		Vector3f pt = new Vector3f();
		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					if ( (x >= min.X) && (x <= max.X) && 
						 (y >= min.Y) && (y <= max.Y) &&
						 (z >= min.Z) && (z <= max.Z)    )
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
	
	private ModelImage registerImages( ModelImage target, ModelImage src )
	{

        AlgorithmRegOAR3D reg3 = new AlgorithmRegOAR3D(target, src, 1, 12, 0, -30, 30,
                15, 6, -30, 30, 15, 6, -30, 30, 15, 6, true, true, true, 
                false, 2, 3);
        reg3.setJTEM(false);
        reg3.run();
        reg3.setRunningInSeparateThread(false);
        

        if (reg3.isCompleted()) {
        	final int xdimA = target.getExtents()[0];
        	final int ydimA = target.getExtents()[1];
        	final int zdimA = target.getExtents()[2];
        	final float xresA = target.getFileInfo(0).getResolutions()[0];
        	final float yresA = target.getFileInfo(0).getResolutions()[1];
        	final float zresA = target.getFileInfo(0).getResolutions()[2];
        	final TransMatrix finalMatrix = reg3.getTransform();


        	final String name = JDialogBase.makeImageName(src.getImageName(), "_register");

        	AlgorithmTransform transform = new AlgorithmTransform(src, finalMatrix, AlgorithmTransform.TRILINEAR, xresA, yresA, zresA, xdimA,
        			ydimA, zdimA, true, false, false);

        	transform.setUpdateOriginFlag(true);
        	transform.setFillValue((float) src.getMin());
        	transform.run();
        	ModelImage resultImage = transform.getTransformedImage();
        	transform.finalize();

        	resultImage.calcMinMax();
        	resultImage.setImageName(name);
        	return resultImage;
        }
        return null;
	}
	
	private void scaleMesh( TriMesh mesh )
	{
        float xRes = srcImage.getFileInfo(0).getResolutions()[0];
        float yRes = srcImage.getFileInfo(0).getResolutions()[1];
        float zRes = srcImage.getFileInfo(0).getResolutions()[2];
        
		Vector3f scale = new Vector3f(offSet/xRes, offSet/yRes, offSet/zRes);
		
		for ( int i = 0; i < mesh.VBuffer.GetVertexQuantity(); i++ )
		{
			Vector3f pos = mesh.VBuffer.GetPosition3(i);
			Vector3f normal = mesh.VBuffer.GetNormal3(i);
			normal.mult( scale );		
			
//			System.err.println(pos);
			pos.add(normal);
//			System.err.println(pos);
//			System.err.println("");
//			System.err.println("");
			mesh.VBuffer.SetPosition3(i, pos);
		}
	}

}
