package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.Vector2d;
import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.filters.AlgorithmGaussianBlurSep;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageCalculator;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageMath;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmMask;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.Point3D;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIPoint;

import gov.nih.mipav.util.CircleUtil;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogExtractBrain;
import gov.nih.mipav.view.dialogs.JDialogMask;

import java.awt.Color;
import java.io.IOException;
import java.util.*;


public class AlgorithmSkullRemoval extends AlgorithmBase
{
    private int faceOrientation;
	private float min_2P = -1;
	private float max_98P = -1;
	private float csfThreshold = -1;
	private float wmThreshold = -1;
	private float gmThreshold = -1;
	private float brainThreshold = -1;
	private float maxRadius = -1;
	private Vector3f cog;
	private int dimX;
	private int dimY;
	private int dimZ;
	private VOI seedPoints;

	private boolean blurFace = false;
	private boolean removeFace = false;
	private boolean showSegmentation = false;
	
    /**
     * Construct the face anonymizer, but do not run it yet.
     *
     * @param  srcImg            The image to de-face
     * @param  faceDirection     the orientation of the patient's face, as determined by the dialog
     */
    public AlgorithmSkullRemoval(ModelImage srcImg, int faceDirection)
    {
    	super( (ModelImage) srcImg.clone(), srcImg );
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
    }
    
    public void setOutputOption( boolean blurFace, boolean removeFace, boolean showSegmentation )
    {
    	this.blurFace = blurFace;
    	this.removeFace = removeFace;
    	this.showSegmentation = showSegmentation;
    }

    
    private void skullRemoval()
    {
        fireProgressStateChanged(0, null, "Estimating parameters ...");

        estimateParameters();
        
        fireProgressStateChanged(30);
        
        maskBrain();       
        new ViewJFrameImage((ModelImage) destImage.clone());
        
        fireProgressStateChanged(60);

        estimateWhiteMatter();
        
        fillWhiteMatter();
        
        calculateRadius();
        
        createVOIs();
        
        fireProgressStateChanged(100);
        
        setCompleted(true);
    }
    
    private void calculateRadius()
    {   	
    	float[] res = srcImage.getResolutions(0);
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
    				if ( destImage.getMask().get(index) )
    				{
    					float value = destImage.getFloat(x,y,z);
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

		float value = destImage.getFloat(maxX,maxY,maxZ);
    	Preferences.debug( "Max Radius " + maxRadius + " location = " + maxX + " " + maxY + " " + maxZ  + "  value = " + value + "\n", Preferences.DEBUG_ALGORITHM );
    	maxRadius = Math.min( maxRadius, Math.min( dimX*res[0], Math.min(dimY*res[1], dimZ*res[2]) ) );
    	maxRadius /= 2f;
    	
    }
    
    private void createVOIs()
    {
    	short id = (short) destImage.getVOIs().getUniqueID();
    	VOI sphere = new VOI(id, "sphere", VOI.CONTOUR, 1f );
    	
    	Vector<Vector3f> centerCog = new Vector<Vector3f>();
    	for ( int z = 0; z < dimZ; z++ )
    	{
			float scaleZ = z / cog.Z;
			if ( z > cog.Z )
			{
				scaleZ = (dimZ - 1 - z)/cog.Z;
			}
			scaleZ += .25f;
			scaleZ = Math.min(1,scaleZ);
			centerCog.add( new Vector3f( cog.X, cog.Y, z) );
    		VOIContour sphereContourZ = new VOIContour( false, true );
    		float radialSamples = 360;
    		for ( int i = 0; i < radialSamples; i++ )
    		{
    			Vector3f pt = new Vector3f( (float)(cog.X + scaleZ*maxRadius * Math.cos( Math.PI * 2.0 * i/radialSamples )),
    					(float)(cog.Y + scaleZ*maxRadius * Math.sin( Math.PI * 2.0 * i/radialSamples)), z );
    			sphereContourZ.add(pt);
    		}
    		sphere.getCurves().add(sphereContourZ);
    	}
    	
//    	destImage.registerVOI(sphere);
    	sphere.setColor( Color.green );



    	
    	id = (short) destImage.getVOIs().getUniqueID();
    	VOI contourWM = new VOI(id, "contourWM", VOI.CONTOUR, 1f );
    	
    	id = (short) destImage.getVOIs().getUniqueID();
    	VOI contourEdge = new VOI(id, "contourEdge", VOI.CONTOUR, 1f );
    	
    	

//    	for ( int z = 0; z < centerCog.size(); z++ )
//    	{
//    		Vector3f center = centerCog.elementAt(z);
//    		VOIContour contourEdgeZ = new VOIContour( false, true );
//    		VOIContour sphereContourZ = (VOIContour) sphere.getCurves().elementAt(z);
//    		for ( int i = 0; i < sphereContourZ.size(); i++ )
//    		{
//    			Vector3f pt = new Vector3f(sphereContourZ.elementAt(i));
//    			Vector3f dir = Vector3f.sub( pt, center );
//    			dir.normalize();    			
//				pt.add(dir);
//    			while ( inBounds(pt) )
//    			{
//    				pt.add(dir);
//    				float value = destImage.getFloat((int)pt.X, (int)pt.Y, (int)pt.Z);
//    				if ( value <= csfThreshold )
//    				{
//    					contourEdgeZ.add(pt);
//    					break;
//    				}
//    			}
//    		}
//    		contourEdgeZ.trimPoints(1.0, true);  		
//    		contourEdge.getCurves().add(contourEdgeZ);
//    	}
    	
    	
    	
    	
    	
    	for ( int z = 0; z < centerCog.size(); z++ )
    	{
    		Vector3f center = centerCog.elementAt(z);

    		VOIContour contourWMZ = new VOIContour( false, true );
    		VOIContour sphereContourZ = (VOIContour) sphere.getCurves().elementAt(z);
    		for ( int i = 0; i < sphereContourZ.size(); i++ )
    		{
    			Vector3f pt = new Vector3f(sphereContourZ.elementAt(i));
    			Vector3f dir = Vector3f.sub( pt, center );
    			dir.normalize();
    			
    			Vector3f lastWMPt = new Vector3f(pt);
    			boolean found = false;
    			while ( inBounds(pt) )
    			{
					int index = (int) ((int)pt.Z * dimX * dimY + (int)pt.Y * dimX + (int)pt.X);
    				if ( destImage.getMask().get(index) )
    				{
    					lastWMPt.copy(pt);
    					found = true;
    				}
    				pt.add(dir);
    			}
    			if ( !found )
    			{
    				dir.neg();
    				pt.copy(lastWMPt);
    				Vector3f checkDir1 = Vector3f.sub( lastWMPt, center ); checkDir1.normalize();
    				Vector3f checkDir2 = Vector3f.sub( pt, center ); checkDir2.normalize();
        			while ( checkDir1.dot(checkDir2) > 0 )
        			{
    					int index = (int) ((int)pt.Z * dimX * dimY + (int)pt.Y * dimX + (int)pt.X);
        				if ( destImage.getMask().get(index) )
        				{
        					lastWMPt.copy(pt);
        					break;
        				}
        				pt.add(dir);
        				checkDir2 = Vector3f.sub( pt, center ); checkDir2.normalize();
        			}    				
    			}
    			pt.copy(lastWMPt);
    			contourWMZ.add(new Vector3f(pt));
    			
//    			while ( inBounds(pt) )
//    			{
//    				float value = destImage.getFloat((int)pt.X, (int)pt.Y, (int)pt.Z);
//    				if ( value <= csfThreshold )
//    				{
//    					contourEdgeZ.add(pt);
//    					break;
//    				}
//    				pt.add(dir);
//    			}
    		}
//    		contourWMZ.trimPoints(0.5, true);
    		
    		contourWM.getCurves().add(contourWMZ);
//    		contourEdge.getCurves().add(contourEdgeZ);
    	}
    	
    	

    	for ( int z = 0; z < centerCog.size(); z++ )
    	{
    		Vector3f center = centerCog.elementAt(z);

    		VOIContour contourWMZ = (VOIContour) contourWM.getCurves().elementAt(z);
    		VOIContour contourEdgeZ = new VOIContour( false, true );
    		for ( int i = 0; i < contourWMZ.size(); i++ )
    		{
    			Vector3f pt = new Vector3f(contourWMZ.elementAt(i));
    			Vector3f dir = Vector3f.sub( pt, center );
    			dir.normalize();
    			    			
    			while ( inBounds(pt) )
    			{
    				float value = destImage.getFloat((int)pt.X, (int)pt.Y, (int)pt.Z);
    				if ( value <= csfThreshold )
    				{
    					contourEdgeZ.add(pt);
    					break;
    				}
    				pt.add(dir);
    			}
    		}
//    		contourEdgeZ.trimPoints(0.5, true);    		    		
    		contourEdge.getCurves().add(contourEdgeZ);
    	}
    	
    	destImage.registerVOI(contourWM);
    	destImage.registerVOI(contourEdge);
    	contourWM.setColor( Color.blue );
    	contourEdge.setColor( Color.magenta );
    	
    	
    	if ( blurFace || removeFace )
    	{
    		contourEdge.setAllActive(true);

            // Make result image of source type
    		ModelImage maskOutside = new ModelImage(destImage.getType(), destImage.getExtents(),
                                         JDialogBase.makeImageName(destImage.getImageName(), "_mask"));
    		AlgorithmMask maskAlgo;
            // Make algorithm[
            if (destImage.isColorImage()) {
                maskAlgo = new AlgorithmMask(maskOutside, destImage, 0, 0, 0, true, true);
            } else {
                maskAlgo = new AlgorithmMask(maskOutside, destImage, 0, true, true);
            }
            maskAlgo.setRunningInSeparateThread(false);
            maskAlgo.run();
//            new ViewJFrameImage(maskOutside);    	

            destImage.clearMask();
            destImage.resetVOIs();

            // Remove face:
            AlgorithmImageCalculator imageCalc = new AlgorithmImageCalculator( destImage, maskOutside,
            		AlgorithmImageCalculator.SUBTRACT, AlgorithmImageMath.CLIP, true, null );
            imageCalc.setRunningInSeparateThread(false);
				imageCalc.run();				
				
			if ( blurFace )
			{
				// Blur face and add it back to the image:
				
		        int index = srcImage.getExtents()[2] / 2;
		        float xRes = srcImage.getFileInfo(index).getResolutions()[0];
		        float zRes = srcImage.getFileInfo(index).getResolutions()[2];

		        float correction = xRes / zRes;

	            final float[] sigmas = {5, 5, 5*correction};
	    		ModelImage blurOutside = new ModelImage(destImage.getType(), destImage.getExtents(),
                        JDialogBase.makeImageName(destImage.getImageName(), "_blur"));
	            AlgorithmGaussianBlurSep gaussianBlurSepAlgo = new AlgorithmGaussianBlurSep(maskOutside, sigmas, true, false);
	            gaussianBlurSepAlgo.setRunningInSeparateThread(false);
	            gaussianBlurSepAlgo.run();

                try {
                	blurOutside.importData(0, gaussianBlurSepAlgo.getResultBuffer(), true);
                } catch (final IOException e) {
                	blurOutside.disposeLocal();
                    MipavUtil.displayError("Algorithm Gausssian Blur importData: Image(s) Locked.");
                    return;
                }
	            
//                new ViewJFrameImage(blurOutside);
	            addBlur( maskOutside, blurOutside, destImage );
			}
    	}
		destImage.calcMinMax();
        new ViewJFrameImage(destImage);    	
    }
    
    private boolean inBounds( Vector3f pt )
    {
		if ( (pt.X >= 0) && (pt.X < dimX) && (pt.Y >= 0) && (pt.Y < dimY) && (pt.Z >= 0) && (pt.Z < dimZ) )
		{
			return true;
		}
		return false;
    }
    
    private void addBlur( ModelImage maskImage, ModelImage blurImage, ModelImage destImage )
    {
    	double min = destImage.getMin();
    	double maskMin = maskImage.getMin();
    	
    	for ( int z = 0; z < dimZ; z++ )
    	{
    		for ( int y = 0; y < dimY; y++ )
    		{
    			for ( int x = 0; x < dimX; x++ )
    			{
    				float value = destImage.getFloat(x,y,z);
    				if ( value == min )
    				{
        				float mask = maskImage.getFloat(x,y,z);
        				if ( mask != maskMin )
        				{
        					destImage.set(x,y,z, blurImage.getFloat(x,y,z) );
        				}
    				}
    			}
    		}
    	}
    	
    }
    
    private void maskBrain()
    {

    	for ( int z = 0; z < dimZ; z++ )
    	{
    		for ( int y = 0; y < dimY; y++ )
    		{
    			for ( int x = 0; x < dimX; x++ )
    			{
    				float value = destImage.getFloat(x,y,z);

    				if ( value <= csfThreshold )
    				{
    					destImage.getMask().set( z * dimX*dimY + y * dimX + x );
    				}
    			}
    		}
    	}
    }
    
    private void fillWhiteMatter()
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
    			Vector3f seed = seedVOI.elementAt(i);    		
        		int index = (int) (seed.Z * dimX * dimY + seed.Y * dimX + seed.X);	
    			visited.set(index);
				whiteMatter.set(index);
    			seedList.add(seed);
    		}
    	}    	
    	
		fill( seedList, visited, whiteMatter, destImage.getMask() );
    	
    	destImage.clearMask();
    	destImage.setMask( whiteMatter );
    }
    
    private void fill( Vector<Vector3f> seedList, BitSet visited, BitSet whiteMatter, BitSet csfMask )
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
									if ( !csfFound )
									{
										float value = destImage.getFloat(x, y, z);
										if ( value > gmThreshold )
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
    
    private void estimateWhiteMatter()
    {
    	int cubeSize = 5;
    	int cubeHalf = cubeSize/2;
    	
    	Vector<Vector2d> cubeIntensities = new Vector<Vector2d>();
    	Vector<Vector3f> cubeCenters = new Vector<Vector3f>();
    	for ( int z = 0; z < dimZ - cubeSize; z+=cubeSize )
    	{
    		for ( int y = 0; y < dimY - cubeSize; y+=cubeSize )
    		{
    			for ( int x = 0; x < dimX - cubeSize; x+=cubeSize )
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
            						if ( destImage.getMask().get(index) )
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
    	
    	int minIndex = -1;
    	double minVar = Double.MAX_VALUE;
    	for ( int i = 0; i < sortedIntensities.length; i++ )
    	{    		
    		if ( (sortedIntensities[i].X > csfThreshold) && ( sortedIntensities[i].Y < minVar) )
    		{
    			minVar = sortedIntensities[i].Y;
    			minIndex = i;
    		}
    	}
    	
   	
    	
    	

    	short id = (short) destImage.getVOIs().getUniqueID();
    	seedPoints = new VOI(id, "SeedPoints", VOI.POINT, 1f );
    	

    	for ( int i = 0; i < cubeCenters.size(); i++ )
    	{
    		if ( (cubeIntensities.elementAt(i).X > csfThreshold) && 
    				(cubeIntensities.elementAt(i).Y == sortedIntensities[minIndex].Y) )
    		{   			
    			seedPoints.importPoint(cubeCenters.elementAt(i));

    			Preferences.debug( "Seed point: " + cubeCenters.elementAt(i) + "       intensity = " + cubeIntensities.elementAt(i).X + " variance = " + cubeIntensities.elementAt(i).Y + "\n", Preferences.DEBUG_ALGORITHM );
    		}
    	}

    	destImage.registerVOI(seedPoints);
    	seedPoints.setColor( Color.red );
    	
    	wmThreshold = (float) sortedIntensities[minIndex].X;
    	gmThreshold = 0.8f * wmThreshold;
    	brainThreshold = 0.36f * wmThreshold;

    	Preferences.debug( "White matter threshold = " + wmThreshold + " variance = " + sortedIntensities[minIndex].Y + "\n", Preferences.DEBUG_ALGORITHM );
    	Preferences.debug( "Gray matter threshold = " + gmThreshold + "\n", Preferences.DEBUG_ALGORITHM );
    	Preferences.debug( "Brain threshold = " + brainThreshold + "\n", Preferences.DEBUG_ALGORITHM );
    	
    	
    	Preferences.debug( "Number of seed points " + seedPoints.getCurves().size() + "\n", Preferences.DEBUG_ALGORITHM );
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
    	int outnt_98P = (int) (0.98 * totalCount);
    	
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
    		if ( !maxFound && (runningCount >= outnt_98P) )
    		{
    			max_98P = keyArray[i];
    			maxFound = true;
    		}
    	}
    	
    	Preferences.debug( "Image min = " + min + " max = " + max + "\n", Preferences.DEBUG_ALGORITHM );
    	Preferences.debug( "Robust min = " + min_2P + " robust max = " + max_98P + "\n", Preferences.DEBUG_ALGORITHM );
    	
    	csfThreshold = .1f * (max_98P - min_2P);
    	Preferences.debug( "CSF Threshold = " + csfThreshold + "\n", Preferences.DEBUG_ALGORITHM );
    	
    	double weightSum = 0;
    	Vector3f pos = new Vector3f();
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
    				else if ( value > csfThreshold)
    				{
    					// value is between min and max and greater than csf threshold
    					pos.X += (x * value);
    					pos.Y += (y * value);
    					pos.Z += (z * value);
    					
    					weightSum += value;
    				}
    				destImage.set(x, y, z, value );
    			}
    		}
    	}

    	pos.X /= weightSum;
    	pos.Y /= weightSum;
    	pos.Z /= weightSum;

    	pos.X = Math.round(pos.X);
    	pos.Y = Math.round(pos.Y);
    	pos.Z = Math.round(pos.Z);

    	cog = new Vector3f(pos);
    	short id = (short) destImage.getVOIs().getUniqueID();
    	VOI centerGravity = new VOI(id, "CenterOfGravity", VOI.POINT, 1f );
    	centerGravity.importPoint(cog);
    	destImage.registerVOI(centerGravity);
    	centerGravity.setColor( Color.green );
    	
    	Preferences.debug( "Center of gravity " + cog + "\n", Preferences.DEBUG_ALGORITHM );   	    	
    }
    
    
    
}
