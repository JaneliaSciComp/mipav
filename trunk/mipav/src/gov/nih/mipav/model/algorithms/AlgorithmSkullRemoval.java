package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.algorithms.filters.OpenCL.filters.OpenCLAlgorithmMarchingCubes;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR3D;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRotate;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogFaceAnonymize;
import gov.nih.mipav.view.renderer.WildMagic.Interface.SurfaceExtractorCubes;

import java.awt.Color;
import java.util.Arrays;
import java.util.BitSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;
import java.util.Vector;

import WildMagic.LibFoundation.Intersection.IntrSegment3Triangle3f;
import WildMagic.LibFoundation.Mathematics.Plane3f;
import WildMagic.LibFoundation.Mathematics.Segment3f;
import WildMagic.LibFoundation.Mathematics.Triangle3f;
import WildMagic.LibFoundation.Mathematics.Vector2d;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Meshes.ConvexHull3f;
import WildMagic.LibGraphics.SceneGraph.BoxBV;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

/**
 * Algorithm Implementation
 * 1. Calculate the input image histogram.
 * 2. Calculate the image values that represent 2% and 98% of the histogram values and clamp the input image data to that range.
 * 3. Set the cerebrospinal fluid (CSF) threshold minimum value to 30 % of the clamped image range and the CSF threshold maximum 
 *    value to 70% of the clamped image range.
 * 4. Calculate the brain center of gravity (COG) as the weighted sum of all voxels between the CSF minimum and maximum range 
 *    using the voxel position scaled by intensity value.
 * 5. Estimate the white matter threshold by segmenting the image into 5x5x5 voxel cubes and finding the cube with the highest 
 *    mean intensity and lowest variance.  The mean intensity of the selected cube is used to set the white matter threshold value.
 * 6. Generate a white-matter segmentation of the brain by using a flood-fill algorithm with the white matter threshold seed 
 *    point as the starting point. The flood-fill algorithm is limited by the CSF threshold values as well as a radius centered 
 *    around the initial seed point.
 * 7. Take the convex-hull of the white matter segmentation and pass this mesh into a modified version of the AlgorithmBrainExtractor.
 * 8. The output of the AlgorithmBrainExtractor is a segmented mesh representing the surface of the brain.  To err on the side 
 *    of not removing too much data, the convex-hull of the brain surface is generated.
 * 9. The final mask representing the data outside the brain is generated using all voxels outside the convex-hull of the brain surface.
 * 10. (Optional final step). If the user chooses to segment only the face region instead of the entire skull region the final step 
 *     produces the face segmentation from the convex-hull of the brain surface.
 */
public class AlgorithmSkullRemoval extends AlgorithmBase
{   
	/** Face Orientation is left: */
	private int faceOrientation = JDialogFaceAnonymize.FACING_LEFT;
	/** Value representing 2% of the histogram count: */
	private float min_2P = -1;
	/** Value representing 98% of the histogram count: */
	private float max_98P = -1;

	/** Cerebrospinal Fluid (CSF) minimum threshold : */
	private float csfThresholdMin = -1;
	/** Cerebrospinal Fluid (CSF) maximum threshold : */
	private float csfThresholdMax = -1;
	/** White Matter threshold : */
	private float wmThreshold = -1;
	/** Gray Matter minimum threshold : */
	private float gmThresholdMin = -1;
	/** Gray Matter maximum threshold : */
	private float gmThresholdMax = -1;

	/** Center of Gravity : */
	private Vector3f cog;
	private int dimX;
	private int dimY;
	private int dimZ;

	/** when true, blur the output */
	private boolean blur = false;
	/** when true, remove the output */
	private boolean remove = false;
	/** when true, compute face segmentation instead of skull */
	private boolean face = false;
	/** when true, display the face segmentation */
	private boolean showFaceSegmentation = false;
	/** when true, display the skull segmentation */
	private boolean showSkullSegmentation = false;

	/** Optional offset (in mm) boundary around the segmented region */
	private float offSet = 0;
	
	/** stores the original image orientation axis */
	private int[] originalAxis;
	/** the orientation the images are remapped to before processing:  */
	private int[] targetAxis = new int[]{FileInfoBase.ORI_A2P_TYPE, FileInfoBase.ORI_S2I_TYPE,
            FileInfoBase.ORI_L2R_TYPE};;

	/** stores the original image orientation axis (used for the target image when an atlas image is present) */
	private int[] originalAxisTargetAtlas;
	/** When true the source image was remapped */
	private boolean reMappedSrc = false;
	/** When true the target image was remapped (used for the target image when an atlas image is present) */
	private boolean reMappedTargetAtlas = false;
	
	/** atlas image: */
	private ModelImage atlasBasedImage;
	/** registered atlas image */
	private ModelImage registeredSourceImage;
	/** original source image */
	private ModelImage originalSrc;
	
    /**
     * Automatic DeSkulling initialization.
     * @param  srcImg  The image to segment.
     */
    public AlgorithmSkullRemoval(ModelImage srcImg)
    {
    	super( null, srcImg );
    	originalSrc = srcImg;

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
        
    	dimX = srcImage.getExtents().length > 0 ? srcImage.getExtents()[0] : 1;
    	dimY = srcImage.getExtents().length > 1 ? srcImage.getExtents()[1] : 1;
    	dimZ = srcImage.getExtents().length > 2 ? srcImage.getExtents()[2] : 1;    	
    }
    

    /**
     * Automatic DeSkulling initialization.
     * @param  srcImg  The image to segment.
     * @param  atlasImage  The atlas image.
     */
    public AlgorithmSkullRemoval(ModelImage srcImg, ModelImage atlasImage)
    {
    	super( null, atlasImage );
    	originalSrc = srcImg;

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
        }
        
    	destImage = (ModelImage) srcImage.clone();
    	destImage.setImageName( srcImage.getImageName() + "_deskull" );
        
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
    	super.setStartTime();
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
    
    /**
     * Set the boundary offset parameter in mm.
     * @param offSet
     */
    public void setOffSet( float offSet )
    {
    	this.offSet = offSet;
    }
    
    /**
     * Set the output options
     * @param blur when true blur the segmented region
     * @param remove when true remove the segmented region
     * @param face when true segment the face
     * @param showFaceSegmentation when true show the face segmentation
     * @param showSkullSegmentation when true show the skull segmentation
     */
    public void setOutputOption( boolean blur, boolean remove, boolean face, boolean showFaceSegmentation, boolean showSkullSegmentation )
    {
    	this.blur = blur;
    	this.remove = remove;
    	this.face = face;
    	this.showFaceSegmentation = showFaceSegmentation;
    	this.showSkullSegmentation = showSkullSegmentation;
    }

    
    /**
     * Combines the randomized-voxel blurred image with the output image.
     * Only the areas that are in the segmented region are modified by the blur.
     * @param blurImage
     * @param destImage
     */
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
     * Compute the convex hull of the input mesh, return the convex hull.
     * @param mesh
     * @return
     */
    private TriMesh convexHull( TriMesh mesh )
    {
		ConvexHull3f convexHull = new ConvexHull3f( mesh.VBuffer.GetVertexQuantity(), mesh.VBuffer.GetPositionArray(), 0.00001f, true );
		IndexBuffer iBuffer = new IndexBuffer(convexHull.GetIndices());
		VertexBuffer vBuffer = new VertexBuffer( mesh.VBuffer );
		TriMesh convexHullMesh = new TriMesh( vBuffer, iBuffer );

		convexHullMesh = removeUnusedVertices(convexHullMesh);
		return convexHullMesh;
    }
    
    private BitSet createMask( Plane3f clipPlane, Vector3f cog )
	{
		Vector3f min = new Vector3f(0, cog.Y, 0);
		Vector3f max = new Vector3f(cog.X, dimY, dimZ);
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
		floodFill( meshMask, cog);
		if ( show )
		{
			ModelImage maskImage = (ModelImage)srcImage.clone();
			maskImage.setMask(meshMask);
			maskImage.setImageName( srcImage.getImageName() + postScript );        	
			new ViewJFrameImage(maskImage);		
		}
		return meshMask;
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
    	return kMesh;

    }
    
    private TriMesh createMesh( ModelImage image )
    {
    	if ( (Preferences.isGpuCompEnabled() && OpenCLAlgorithmBase.isOCLAvailable()) )
    	{
    		OpenCLAlgorithmMarchingCubes cubesAlg = new OpenCLAlgorithmMarchingCubes(image, 0, false,
    				false, false, 0, null /*JDialogBase.makeImageName(image.getImageName(), ".sur") */ );
    		cubesAlg.setRunningInSeparateThread(false);
    		cubesAlg.run();
    		return cubesAlg.getMesh();  
    	}
    	return createMesh(image.getMask());
    }

    
    private float distance( Vector3f cog, int x, int y, int z )
    {
        float xRes = srcImage.getFileInfo(0).getResolutions()[0];
        float yRes = srcImage.getFileInfo(0).getResolutions()[1];
        float zRes = srcImage.getFileInfo(0).getResolutions()[2];
        Vector3f temp = new Vector3f( cog.X*xRes,cog.Y*yRes,cog.Z*zRes);
    	float dist = temp.distance( new Vector3f(x*xRes,y*yRes,z*zRes) );
    	return dist;
    }
    
    /**
     * 
     * 1. Calculate the input image histogram.
     * 2. Calculate the image values that represent 2% and 98% of the histogram values and clamp the input image data to that range.
     * 3. Set the cerebrospinal fluid (CSF) threshold minimum value to 30 % of the clamped image range and the CSF threshold maximum 
     *    value to 70% of the clamped image range.
     * 4. Calculate the brain center of gravity (COG) as the weighted sum of all voxels between the CSF minimum and maximum range 
     *    using the voxel position scaled by intensity value.
     * Steps 1-4: The preprocessed steps generate the center of gravity (COG) and cerebrospinal fluid (CSF) thresholds, 
     * which are used to find the initial white-matter seed point.  Potential white-matter seed points are limited to 
     * a radius around the COG.  The minimum variance is selected from potential seed points with a maximum mean-intensity 
     * greater than the CSF threshold.  These steps prevent the algorithm from finding a seed point in the background, 
     * where the variance may be zero but where the intensity is lower than the CSF threshold.
     */
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
    	Vector3f posMin = new Vector3f();
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
    					if ( (value > csfThresholdMin) && (value < csfThresholdMax) )
    					{
    						// value is between min and max and greater than csf threshold
    						posMin.X += (x * value);
    						posMin.Y += (y * value);
    						posMin.Z += (z * value);

    						weightSumMin += value;
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

		float value = destImage.getFloat((int)posMin.X, (int)posMin.Y, (int)posMin.Z);
        Preferences.debug( "CSF min = " + posMin + "   value = " + value + "\n", Preferences.DEBUG_ALGORITHM );
    	cog = new Vector3f(posMin); 	    	
    }
    
    
    /**
     * 5. Estimate the white matter threshold by segmenting the image into 5x5x5 voxel cubes and finding the cube with the highest 
     *    mean intensity and lowest variance.  The mean intensity of the selected cube is used to set the white matter threshold value.
     * @param image image remapped to the robust image range.
     * @return initial white-matter seed point(s).
     */
    private VOI estimateWhiteMatter( ModelImage image )
    {
    	Vector3f min = new Vector3f(dimX, dimY, dimZ);
    	Vector3f max = new Vector3f(0,0,0);
		getBoundsDistance( min, max, cog, 60, 60 );
		
    	int cubeSize = 5;
    	int cubeHalf = cubeSize/2;
    	
    	Vector<Vector2d> cubeIntensities = new Vector<Vector2d>();
    	Vector<Vector3f> cubeCenters = new Vector<Vector3f>();
    	// Shifting 5x5x5 voxel cube through the volume, calculating the mean intesity and variance:
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
    				meanIntensity /= count;

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
    				meanVariance /= count;
    				cubeIntensities.add(new Vector2d( meanIntensity, meanVariance) );

    				Vector3f center = new Vector3f( x + cubeHalf, y + cubeHalf, z + cubeHalf );
    				cubeCenters.add(center);
    			}
    		}
    	}
    	
    	// Sort the intensities:
    	Vector2d[] sortedIntensities = new Vector2d[cubeIntensities.size()];
    	for ( int i = 0; i < cubeIntensities.size(); i++ )
    	{
    		sortedIntensities[i] = cubeIntensities.elementAt(i);
    	}
    	Arrays.sort( sortedIntensities );
    	
    	int minIndex = -1;
    	double minVar = Double.MAX_VALUE;
    	// Find the minimum variance, must be with an average intesnsity > CSF min:
    	for ( int i = 0; i < sortedIntensities.length; i++ )
    	{    		
    		if ( srcImage.getImageModality() == FileInfoBase.COMPUTED_TOMOGRAPHY ) 
    		{ 
    			if ( (sortedIntensities[i].X > csfThresholdMin) && (sortedIntensities[i].X < csfThresholdMax) && 
    					( sortedIntensities[i].Y < minVar) )
    			{
    				minVar = sortedIntensities[i].Y;
    				minIndex = i;
    			}
    		}
    		else if ( (sortedIntensities[i].X > csfThresholdMin) && (sortedIntensities[i].Y < minVar) )
    		{
    			minVar = sortedIntensities[i].Y;
    			minIndex = i;
    		}
    	}
    	
    	double maxIntensity = -Double.MAX_VALUE;
    	// Find the maximum mean intensity:
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

    	short id = (short) image.getVOIs().getUniqueID();
    	VOI seedPoints = new VOI(id, "SeedPoints", VOI.POINT, 1f );
    	
    	// The seed point is the location with maximum intensity. If there are more
    	// than one with the maximum mean intensity, take the one with the lowest variance:
    	for ( int i = 0; i < cubeCenters.size(); i++ )
    	{
    		if ( cubeIntensities.elementAt(i).X == maxIntensity && cubeIntensities.elementAt(i).Y <= minVar )
    		{	
//    			System.err.println( "Seed point: " + cubeCenters.elementAt(i) + "       intensity = " + cubeIntensities.elementAt(i).X + " variance = " + cubeIntensities.elementAt(i).Y );
    			seedPoints.importPoint(cubeCenters.elementAt(i));
    		}
    	}

    	image.registerVOI(seedPoints);
    	seedPoints.setColor( Color.red );
    	
    	// Set the white matter threshold value:
    	wmThreshold = (float) sortedIntensities[minIndex].X;
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
        
    private void fill( ModelImage image, Vector3f centerBound, Vector<Vector3f> seedList, BitSet visited, BitSet whiteMatter )
    {
    	int radiusMax = 110;
    	int radiusMin = 90;
    	Vector3f min = new Vector3f(dimX, dimY, dimZ);
    	Vector3f max = new Vector3f(0,0,0);
		getBoundsDistance( min, max, centerBound, radiusMax, radiusMin );
    	while ( seedList.size() > 0 )
    	{
    		Vector3f seed = seedList.remove(0);

    		boolean csfFound = false;
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
								int index = (z * dimX * dimY + y * dimX + x);

								if ( !visited.get(index) )
								{
									visited.set(index);
									if ( (x >= min.X) && (x <= max.X) && (y >= min.Y) && (y <= max.Y) && (z >= min.Z) && (z <= max.Z) )
									{
										if ( !csfFound )
										{
											if ( distance( centerBound, x, y, z ) < radiusMax )
											{
												float value = image.getFloat(x, y, z);
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
    		if ( inBounds( x, y, z ) )
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
    		if ( inBounds( x, y, z ) )
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
    		if ( inBounds( x, y, z ) )
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
    		if ( inBounds( x, y, z ) )
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
    		if ( inBounds( x, y, z ) )
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
	
	/**
     * 
     * Once the white-matter seed point is found it is used to initialize a flood-fill algorithm that segments 
     * the white-matter of the brain.  The seed point is typically found in one hemisphere of the brain. The first 
     * step in initializing the flood-fill algorithm is to generate a mirror-image seed-point located in the 
     * opposite hemisphere. This enables the flood-fill algorithm to work equally well on both hemispheres.  
     * The flood-fill algorithm is limited to an ellipsoid shape around the COG, where the long axis of the ellipsoid 
     * is anterior-to-posterior and the short axes of the ellipsoid are superior-to-inferior and left-to-right.
     * 
     * @param image
     * @param seedPoints
     */
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
    			
    			int diffZ = (int) Math.abs(seed.Z - cog.Z);
    			if ( diffZ == 0 )
    			{
    				continue;
    			}
				Vector3f newSeed = new Vector3f(seed);
    			if ( seed.Z - cog.Z > 0 )
    			{
    				newSeed.Z = cog.Z - diffZ;
    			}
    			else
    			{
    				newSeed.Z = cog.Z + diffZ;
    			}		
        		index = (int) (newSeed.Z * dimX * dimY + newSeed.Y * dimX + newSeed.X);	
    			visited.set(index);
				whiteMatter.set(index);
    			seedList.add(newSeed);
    		}
    	}    	
    	// Take first seed point, use x,y but center z:
    	Vector3f centerBound = new Vector3f(seedPoints.getCurves().elementAt(0).elementAt(0));
    	centerBound.X = cog.X;
    	centerBound.Z = cog.Z;
		fill( image, centerBound, seedList, visited, whiteMatter );
    	
		image.setMask( whiteMatter );
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

    /**
	 * Algorithm Implementation Steps:
	 * 7. Take the convex-hull of the white matter segmentation and pass this mesh into a modified version of the AlgorithmBrainExtractor.
	 * 8. The output of the AlgorithmBrainExtractor is a segmented mesh representing the surface of the brain.  To err on the side of 
	 * not removing too much data, the convex-hull of the brain surface is generated.
	 * 9. The final mask representing the data outside the brain is generated using all voxels outside the convex-hull of 
	 * the brain surface.
	 * 10. (Optional final step). If the user chooses to segment only the face region instead of the entire skull region the 
	 * final step produces the face segmentation from the convex-hull of the brain surface.
	 * 
	 * Once the white-matter has been segmented, a convex-hull of the segmentation is generated in step 7. This mesh is passed 
	 * to a modified version of the AlgorithmBrainExtractor in MIPAV.  The convex-hull of the white-matter often provides a 
	 * good enough segmentation of the skull, but in a few cases the convex-hull of the white-matter does not include the 
	 * entire surface of the brain and processing the mesh with the  AlgorithmBrainExtractor corrects the mesh.  

	 * Steps 8-9 were added to provide a buffer around the brain segmentation. This enables the algorithm to err on the side 
	 * of removing too little data rather than removing small portions of the brain surface.
	 * 
	 */
	private void generateConvexHullMask(ModelImage image) 
	{			
		TriMesh whiteMatterMesh = createMesh( image.getMask() );    	
    	BitSet meshCHMask = new BitSet();
		if ( whiteMatterMesh != null )
		{
			TriMesh convexHullMesh = convexHull(whiteMatterMesh);
			meshCHMask = createMask( convexHullMesh, false, "_ch_mask" );
    		image.setMask(meshCHMask);

			TriMesh kMesh = createMesh( image.getMask() );	    	
			if ( kMesh != null )
			{
				AlgorithmBrainExtractor betAlg = new AlgorithmBrainExtractor( image, faceOrientation, kMesh, 
						(int)getMedianIntensity( image, meshCHMask ), cog);
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
				}				
				kMesh = convexHullBrain;		
				
				if ( !(face || showFaceSegmentation) )
				{
					image.setMask( createMask( convexHullBrain, false, "ch_bet_mask" ) );
				}
				
				// Calculate the face segmentation: Step 10. If the user chooses to segment only the face region, 
				// the face mask is generated from the convex-hull of the brain. To generate the face mask, the center of 
				// gravity (COG) is used to generate two points on the surface of the mesh.  A line parallel to the x-axis 
				// centered at the COG intersects the convex-hull, and the intersection point farthest from the COG is selected 
				// as the first point on the surface.  A second line parallel to the y-axis and centered at the COG intersects the 
				// convex-hull and the intersection point farthest from the COG is selected as the second point on the surface. 
				//
				// The two points on the hull define a clipping plane that intersects and may cut through the convex-hull. 
				// The clipping plane is then moved through the convex-hull to find the point where it is tangent to the surface 
				// and does not cut through the surface.  The plane is truncated where it intersects the two lines from the COG 
				// parallel to the x and y-axes.  Truncating the plane prevents the spinal column from being clipped.
				if ( face || showFaceSegmentation )
				{
					Vector3f front = new Vector3f( 0, cog.Y, cog.Z );
					Vector3f bottom = new Vector3f( cog.X, dimY, cog.Z );
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
					Plane3f clipPlane = new Plane3f( new Vector3f( minDistFrontPoint.X, minDistFrontPoint.Y, 0),
							new Vector3f( minDistBottomPoint.X, minDistBottomPoint.Y, 0) , 
							new Vector3f( minDistFrontPoint.X, minDistFrontPoint.Y, dimZ));
					
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
					}
					if ( minIndex != -1 )
					{
						Vector3f pos = kMesh.VBuffer.GetPosition3(minIndex);						
						clipPlane = new Plane3f( clipPlane.Normal, pos );
						BitSet clipMask = createMask( clipPlane, cog );
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
			image.getMask().or(meshCHMask);
	        image.getMask().flip(0, dimX*dimY*dimZ);
		}
	}
    
    
    
    private void getBoundsDistance( Vector3f min, Vector3f max, Vector3f centerBound, float limitX, float limit )
    {
        float xRes = srcImage.getFileInfo(0).getResolutions()[0];
        float yRes = srcImage.getFileInfo(0).getResolutions()[1];
        float zRes = srcImage.getFileInfo(0).getResolutions()[2];
        Vector3f range = new Vector3f( limitX/xRes, limit/yRes, limit/zRes );
    	Vector3f pos = new Vector3f(centerBound);

    	min.min( Vector3f.sub( pos, range ) );
    	max.max( Vector3f.add( pos, range ) );
    		
    	min.max(Vector3f.ZERO);
    	max.min(new Vector3f(dimX,dimY,dimZ));
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
    				int index = ( z * dimX*dimY + y * dimX + x );
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

    private boolean inBounds( int x, int y, int z )
    {
		if ( (x >= 0) && (x < dimX) && (y >= 0) && (y < dimY) && (z >= 0) && (z < dimZ) )
		{
			return true;
		}
		return false;
    }
    
    /**
     * Randomizes voxel cubes with similar average intensities.
     * @param image
     * @param stepSize
     * @return
     */
    private ModelImage randomize( ModelImage image, int stepSize )
	{
		int dimX = image.getExtents()[0];
		int dimY = image.getExtents()[1];
		
		ModelImage blurImage = (ModelImage)image.clone();
		blurImage.setImageName( JDialogBase.makeImageName(image.getImageName(), "_blur") );
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
					boolean useBlock = true;
					for ( int z1 = z; z1 < z + stepSize; z1++ )
					{					
						int srcY = y * stepSize;
						for ( int y1 = y; y1 < y + stepSize; y1++ )
						{	
							int srcX = x * stepSize;
							for ( int x1 = x; x1 < x + stepSize; x1++ )
							{
								if ( !image.getMask().get( srcZ*dimY*dimX + srcY*dimX + srcX) )
								{
									useBlock = false;
								}
								sum += image.getFloat(srcX++, srcY, srcZ);
								count++;
							}							
							srcY++;
						}						
						srcZ++;
					}
					sum /= (count);
					
					int blockIndex = z * stepsY * stepsX + y * stepsX + x;	
					blockAvg[blockIndex] = sum;
					if ( !useBlock )
					{
						blockAvg[blockIndex] = -1;
					}
				}
			}
		}
		
		// randomly swap blocks -- choose blocks that are with in 25% of each other:
		boolean[] swapped = new boolean[length];
		int[] blockIndex = new int[length];
		for ( int i = 0; i < length; i++ )
		{
			if ( blockAvg[i] == -1 )
			{
				swapped[i] = true;
			}
			swapped[i] = false;				
			blockIndex[i] = i;
		}
		for ( int i = 0; i < length; i++ )
		{
			if ( swapped[i] )
				continue;
			for ( int j = length-1; j > i; j-- )
			{
				if ( !swapped[j] && ((Math.abs(blockAvg[i] - blockAvg[j]) < .30*blockAvg[i]) || (Math.abs(blockAvg[i] - blockAvg[j]) < .30*blockAvg[j])))
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
//								if ( image.getMask().get( z1*dimY*dimX + y1*dimX + x1) &&
//										image.getMask().get( srcZ*dimY*dimX + srcY*dimX + srcX))
								{
									//								System.err.println( x1 + " " + y1 + " " + z1 + "      " + srcX + " " + srcY + " " + srcZ );
									blurImage.set( x1,y1,z1, image.getFloat(srcX, srcY, srcZ) );
								}
								srcX++;
							}							
							srcY++;
						}						
						srcZ++;
					}
				}
			}
		}
		
		blurImage.calcMinMax();
//		new ViewJFrameImage((ModelImage)blurImage.clone());
		return blurImage;
	}

	

	/**
	 * Registers the atlas image to the input target image:
	 * @param target
	 * @param src
	 * @return
	 */
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
	
	/**
	 * Scales the input mesh based on the offset parameter set in the constructor.
	 * This enables the user to set a buffer around the segmentation.
	 * @param mesh
	 */
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
			pos.add(normal);
			mesh.VBuffer.SetPosition3(i, pos);
		}
	}
	
	/**
	 * Main algorithm implementation, skull segmentation and removal.
	 */
	private void skullRemoval()
    {
        fireProgressStateChanged(0, null, "Estimating parameters ...");
        // Estimate the initial image parameters: histogram, CSF thresholds, COG:
        estimateParameters();
        
        fireProgressStateChanged(30);
        
        // Using the image parameters find the initial white-matter seed point:
        VOI seedPoints = estimateWhiteMatter(destImage); 
    	
        // Flood-fill to generate a white-matter segmentation:
    	gmThresholdMax = max_98P;
        fillWhiteMatter(destImage, seedPoints);
        
        // Display white-matter segmentation:
        if ( showFaceSegmentation | showSkullSegmentation )
        {
    		destImage.calcMinMax();
        	ModelImage whiteMatterImage = (ModelImage)destImage.clone();
        	whiteMatterImage.setImageName( srcImage.getImageName() + "_whiteMatter" );
        	new ViewJFrameImage(whiteMatterImage);
        }
        fireProgressStateChanged(60);                
        
        // Generate the convex-hull of the white-matter segmentation
        // and call the BET algorithm to produce the final segmentation:
        generateConvexHullMask(destImage);
        
        fireProgressStateChanged(90);
        
        // If the input image was re-oriented to face left, put the destImage in the original orientation:
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

        // If an atlas image was used, apply the atlas image segmentation to the input image segmentation:
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
    	}

    	BitSet mask = (BitSet)(destImage.getMask().clone());
    	destImage.disposeLocal();
    	destImage = (ModelImage)originalSrc.clone();
    	destImage.setMask(mask);
    	if ( remove )
    	{
    		subtractMask( destImage );
    		destImage.setImageName( srcImage.getImageName() + "_deskull" );
    	}

    	if ( blur )
    	{
    		ModelImage pixelizedImage = randomize( destImage, 15 );	        	            
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
	
	/**
	 * Removes portions of the image that are masked.
	 * @param image
	 */
	private void subtractMask( ModelImage image )
    {
		float min = (float) image.getMin();
    	for ( int z = 0; z < dimZ; z++ )
    	{
    		for ( int y = 0; y < dimY; y++ )
    		{
    			for ( int x = 0; x < dimX; x++ )
    			{
					int index = z * dimX * dimY + y * dimX + x;
					if ( image.getMask().get(index) )
					{
						image.set( index, min );
					}
    			}
    		}
    	}
    }

}
