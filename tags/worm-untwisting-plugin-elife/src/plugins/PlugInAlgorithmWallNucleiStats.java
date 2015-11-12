import java.io.IOException;
import java.util.*;

import WildMagic.LibFoundation.Mathematics.Vector2d;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageCalculator;
import gov.nih.mipav.model.structures.*;


/**
 * Plugin for the Collins' Lab to calculate statistics for histology slides. 
 * This is the first iteration of the plugin. In this, the user is required 
 * to manually segment the wall from the histology slide, and pass that in 
 * to the plugin, which will calculate statistics for both the wall section 
 * and the nuclei in the wall. There is minimal error checking at the current 
 * juncture, and the progress bar is not updating smoothly. Further iterations 
 * will contain more error checking, while the progress bar is a lower priority 
 * issue. 
 * 
 * @author wangvg
 *
 */


public class PlugInAlgorithmWallNucleiStats extends AlgorithmBase{
	
	/** Wall Image */
	private ModelImage rgbImage;
	
	/** Mask Image*/
	private ModelImage boolImage;
	
	/** Foreground Distance Image*/
	private ModelImage dImage;
	
	/** Green Channel extracted from the Wall Image*/
	private ModelImage gImage;
	
	/** Used to exclude pixels in the second pass of the midline finding step
	 *  and obtain the midline without branches */
	private ModelImage tempImage;
	
	private int area;
	
	private int imageWidth;
	
	private int imageHeight;
	
	private int length;
	
	private int maxIndex;
	
	private int[] extents;
	
	private float[] stats;
	
	private float[] nStats;
	
	
    public PlugInAlgorithmWallNucleiStats(ModelImage rgb, ModelImage bool) {
        super(null, null);
        rgbImage = rgb;
        boolImage = bool;
        extents = rgb.getExtents();
        imageWidth = extents[0];
        imageHeight = extents[1];
        length = extents[0]*extents[1];
        tempImage = new ModelImage(ModelImage.UBYTE, extents, "Midline Mask");
        stats = new float[4];
        nStats = new float[6];

    }
    
    public void finalize() {

    	this.rgbImage = null;
    	this.boolImage = null;
    	this.dImage = null;
    	this.gImage = null;
    	this.tempImage = null;
        super.finalize();
    }
    
    public int getArea(){
    	return this.area;
    }
    
    public int getMaxIndex(){
    	return this.maxIndex;
    }
    
    public float[] getNucleiStats(){
    	return this.nStats;
    }
    
    public float[] getWallStats(){
    	return this.stats;
    }
    
	@Override
	public void runAlgorithm() {
		
		if (rgbImage == null) {
            displayError("There is no rgb image");

            return;
        }

        if (boolImage == null) {
            displayError("There is no mask image");

            return;
        }
        
        segNuclei();
        
        //Extract statistics from nuclei: (#, area, average size, std. dev, min, max)
        
        int[] gBuffer = new int[length];
        try{
        	gImage.exportData(0, length, gBuffer);
        	Arrays.sort(gBuffer);
            nucleiStatistics(gBuffer);
        } catch(IOException x){//Maybe error handle this as well
        	displayError("Could not export data for nuclei calculations");
        	return;
        }
        
        processMask();
        
        //Find midline from thresholded laplacian image
		//Visited buffer, initially all 0
        if (maxIndex != -1){
			int[] maskBuffer = new int[length];
			
			//First pass
			midline(maskBuffer, maxIndex, 0);
	
			//Second pass
			midline(maskBuffer, maxIndex, 1);
	
			maskBuffer = null;
			
			//Mask the foreground distance image so only the midline is left
			AlgorithmImageCalculator combine = new AlgorithmImageCalculator(dImage, tempImage,
					AlgorithmImageCalculator.MULTIPLY, AlgorithmImageCalculator.CLIP, true, "");
			combine.run();
			
			//Calculate statistics for the wall thickness
			try{
				float[] dBuffer = new float[length];
				dImage.exportData(0, length, dBuffer);
				statistics(dBuffer);
			} catch(IOException x){ //Maybe error handle this?
				displayError("Could not export data for wall thickness calculations");
				return;
			}
        }
		
		dImage = null;
		gImage = null;
		tempImage = null;
		rgbImage = null;
		boolImage = null;
		setCompleted(true);
		
	}
	
	private void area(){
		this.area = 0;
		for(int i=0; i<length; i++){
			if (boolImage.get(i).intValue() == 1) this.area++;
		}
	}
	
	private void maxIndex(){
		float maxPixel = (float)boolImage.getMax();
		for(int i = 0; i<length; i++){
			if (boolImage.get(i).floatValue() == maxPixel){
				this.maxIndex = i;
				return;
			}
		}
		
		this.maxIndex = -1;
	}
	
	/**
	 * Method to extract the midline from the processed mask image. It is a two-pass system. 
	 * Essentially the method searches for the longest continuous path in the image, which 
	 * is achieved by using a continuously expanding wave on the first pass, and then building 
	 * back to the seed point once the wave can no longer expand. This must be done twice to 
	 * build the forward and backwards direction.
	 * 
	 * @param buffer : a boolean image of the processed mask with branches
	 * @param seed : point from which to start the propagations to find the longest paths
	 * @param pass : determines 1st or 2nd iteration of the method to determine which points 
	 * to consider
	 */
	private void midline(int[] buffer, int seed, int pass){ 
		
		//Maximum number of elements in the Deque/Vector
		final int MAXNUM = 200;
		
		//Neighboring pixels to consider
		Vector<Vector2d> dir = new Vector<Vector2d>();
		dir.add(new Vector2d(0, -1));
		dir.add(new Vector2d(1, 0));
		dir.add(new Vector2d(0, 1));
		dir.add(new Vector2d(-1, 0));

		//Which direction to go on the inverse pass
		int[] dirImage = new int[length];
		
		//In the direction image, 1 means it points up, 2 points right,
		//4 points down, and 8 points down. Essentially a bitshift, except
		//the direction is reversed to accommodate the fact that the 
		//pixel you are adding the direction to points BACK at the given pixel
		int[] powers = {4, 8, 1, 2};

		//Set up pool as an ArrayDeque of Vector2d to keep track of which pixels are
		//in the current wave (should change to QUEUE, since double ended structure 
		//isn't needed)
		ArrayDeque<Integer> pool = new ArrayDeque<Integer>(MAXNUM);
		Vector<Integer> prevPool = new Vector<Integer>(MAXNUM);
		Integer index = new Integer(seed);
		buffer[seed] = 1+pass;
		pool.addFirst(index);
		int poolSize = 1;
		int x,y;
		Vector2d coord = new Vector2d();
		Vector2d dirCoord = new Vector2d();
		//int cnt = 0;

		//Propagate waves until terminus

		while(poolSize > 0){

			prevPool.clear();

			//Add next wave while simultaneously removing previous wave
			for(int i=0; i<poolSize; i++){
				//Remove an element from the wave to check for connections
				index = pool.removeFirst();
				x = index % imageWidth;
				y = index / imageWidth;
				coord = new Vector2d(x, y);
				prevPool.add(index);
				for(int j=0; j<4; j++){
					//Do not check connections if it already points to that pixel
					if (((1<<j) & (dirImage[x + y * imageWidth])) != 0) 
						continue; 
					dirCoord = Vector2d.add(coord, dir.elementAt(j));
					if ((dirCoord.X < 0) || (dirCoord.X >= imageWidth)
							|| (dirCoord.Y < 0) || (dirCoord.Y >= imageHeight)){
						continue;
					}
					index = new Integer((int)(dirCoord.X + dirCoord.Y * imageWidth));
					//Check to see if the pixel has already been included in the midline mask
					//and that it is a connection to make
					if ((tempImage.get(index.intValue()).intValue() == 0)
							&& (boolImage.get(index.intValue()).intValue() == 1)){
						dirImage[index.intValue()] += powers[j];
						if(buffer[index.intValue()] == pass){
							//Only add the pixel to the next wave if it hasn't already been checked
							pool.addLast(index);
							buffer[index.intValue()] = 1 + pass;
							
						}
					}
					
				}		
					
			}
			poolSize = pool.size();

		} //poolSize = 0 means the terminus has been reached

		//prevPool contains final terminus
		//Now propagate waves back to seed point
		
		//Place terminus points into the pool
		pool.clear();
		for(int i=0; i<prevPool.size(); i++){
			pool.addLast(prevPool.elementAt(i));
			buffer[prevPool.elementAt(i).intValue()] = 0;
		}
		prevPool.clear();
		poolSize = pool.size();
		int direction;

		while(poolSize > 0){
			//Add next wave while simultaneously removing previous wave
			for(int i=0; i<poolSize; i++){
				index = pool.removeFirst();
				x = index.intValue() % imageWidth;
				y = index.intValue() / imageWidth;
				coord = new Vector2d(x, y);
				direction = dirImage[index.intValue()];
				tempImage.set(index.intValue(), 1);
				for(int j=0; j<4; j++){
					//Check to see if this pixel points in a given direction
					if((direction & 1<<j) == 1<<j){
						dirCoord = Vector2d.add(coord, dir.elementAt(j));
						index = new Integer((int)(dirCoord.X + dirCoord.Y * imageWidth));
						//Only add to the mask if it has not already present
						if (buffer[index.intValue()] != 0){
							pool.addLast(index);
							buffer[index.intValue()] = 0;
						}
					}			
				}
			}
			poolSize = pool.size();

		} //poolSize = 0 means the seed has been reached
	}
	/**
	 * Method for calculating the statistics involving the wall. The order of statistics
	 * in the output array is:
	 * 1) Min 2) Max 3) Average 4) Standard Deviation (all of the thickness)
	 * @param imBuffer : image buffer resulting from the processed mask image. This should
	 * be the distance map image masked to only reveal the midline (no branches)
	 */
	private void statistics(float[] imBuffer){
		float eSquare = 0f;
		float num = 0f;
		stats[0] = 9999f; //Minimum wall thickness
		stats[1] = -9999f; //Maximum wall thickness
		stats[2] = 0; //Average wall thickness
		stats[3] = 0; //Standard deviation
		
		for(int i=0; i<length; i++){
			if (imBuffer[i] !=0){
				num += 1.0f;
				stats[2] += imBuffer[i];
				eSquare += imBuffer[i]*imBuffer[i];
				if (imBuffer[i] < stats[0]) stats[0] = imBuffer[i];
				if (imBuffer[i] > stats[1]) stats[1] = imBuffer[i];
			}
		}
		stats[2] /= num;
		eSquare /= num;
		stats[3] = (float) Math.sqrt(eSquare - stats[2]*stats[2]);
	}
	
	/**
	 * Method for calculating the statistics involving the nuclei. The order of statistics
	 * in the output array is:
	 * 1) Number 2) Total Area 3) Average Size 4) Standard Deviation 5) Minimum Size 6) Maximum Size
	 * @param imBuffer : image buffer resulting from ID objects, should be sorted already
	 */
	private void nucleiStatistics(int[] imBuffer){
		
		int squared = 0; //Keep track of a nucleus size to calculate std. dev
		int prev = 1;
		int num = 0;
		int squaredSum = 0;
		int min = 5000;
		int max = 0;
		
		for(int i=0; i<4; i++) nStats[i] = 0.0f;
		
		for(int i=0; i<length; i++){
			if(imBuffer[i] != 0){
				num++;
				if(imBuffer[i] == prev) squared++;
				else{
					if (squared < min) min = squared;
					else if (squared > max) max = squared;
					prev = imBuffer[i];
					squaredSum += (squared*squared);
					squared = 1;
				}
			}
		}
		
		squaredSum += (squared*squared);
		
		nStats[0] = (float) imBuffer[length-1]; //Number of nuclei
		nStats[1] = (float) num; //Total area of nuclei
		nStats[2] = nStats[1] / nStats[0]; //Average size of nuclei
		//Standard deviation
		nStats[3] = (float) Math.sqrt(((float)squaredSum)/nStats[0] - nStats[2]*nStats[2]);
		
		nStats[4] = min;
		nStats[5] = max;
		
	}
	
	/**
	 * Method for processing the mask image to determine wall statistics. Unlike the segNuclei
	 * method, this does not actually calculate any statistics. The processing steps are:
	 * Foreground distance map -> Laplace filter -> Threshold
	 * 
	 * This should pull out the general midline image although there are branches to remove, 
	 * which are removed later via the midline method.
	 */
	
	private void processMask(){
		
		area();
        
		float[] sigmas = {1, 1, 1};
		float[] threshold = new float[2];
		
		fireProgressStateChanged("Calculating Wall Thickness Statistics: \n Calculating Distance Map");
		
		//Generate foreground distance map to determine where general midline is
        AlgorithmMorphology2D distMap = new AlgorithmMorphology2D(boolImage, AlgorithmMorphology2D.CONNECTED4,
        		1.0f, AlgorithmMorphology2D.DISTANCE_MAP, 0, 0, 0, 0, true);
        
        linkProgressToAlgorithm(distMap);
        distMap.setProgressValues(33, 67);
        distMap.run(); 
        delinkProgressToAlgorithm(distMap);
        dImage = (ModelImage) boolImage.clone("Distance Map");
       
        //Find the largest distance. This SHOULD lie along the midline
        maxIndex();
 
        fireProgressStateChanged("Calculating Wall Thickness Statistics: \n Applying Laplace filter");
        
        //Separate out the midline from the rest of the distance image via a thresholded laplacian
        AlgorithmLaplacian laplace = new AlgorithmLaplacian(boolImage, sigmas, true, false, 1.0f);
        linkProgressToAlgorithm(laplace);
        laplace.setProgressValues(67, 100);
        laplace.run();
       
        threshold[1] = (float) boolImage.getMax();
        threshold[0] = threshold[1] * 0.3f;
        
        AlgorithmThresholdDual thresh = new AlgorithmThresholdDual(boolImage, threshold, 1, 1, true, false);
        thresh.run();
	}
	
	/** 
	 * Method for processing the original wall image, as well as calculating the
	 * nuclei statistics (number, area, average size, std. deviation). The steps are:
	 * Mask -> Hard C-Means -> Threshold -> Fill Holes -> ID Objects -> Generate statistics
	**/
	private void segNuclei(){
		
		gImage = new ModelImage(ModelImage.UBYTE, extents, "GrayG");
		
		int min = 1;
		int max = -9999;
		int gPix;
		
		//Extract only the green channel and calculate the min and max non-background values
		for(int i=0; i<length; i++){
			gPix = rgbImage.getC(i,2).intValue();
			gImage.set(i, gPix);
			if(gPix < min) min = gPix;
			else if (gPix > max) max = gPix;
		}
		
		//Mask the image to only display the wall section
		AlgorithmImageCalculator maskWall = new AlgorithmImageCalculator(gImage, boolImage,
				AlgorithmImageCalculator.MULTIPLY, AlgorithmImageCalculator.CLIP, true, "");
		maskWall.run();

		//Evenly spaced initial centroid guesses
		float[] centroids = new float[5];
		float inc = (float)(max-min)/6.0f;
		for(int i=0; i<5; i++){
			centroids[i] = (float)min + (i+1.0f)*inc;
		}

		fireProgressStateChanged("Calculating Nuclei Statistics");
		setProgressValues(0, 33);
		
		
		AlgorithmFuzzyCMeans cMeans = new AlgorithmFuzzyCMeans(gImage, 5, 0, 0, 0, 2.0f , 0.0f, 0.0f, false, 
				AlgorithmFuzzyCMeans.HARD_ONLY, true, 1.0f, 500, 0.01f, true);
		cMeans.setCentroids(centroids);
		cMeans.run();
		
		//Threshold the nuclei out
		float[] threshold = {1, 1};
        AlgorithmThresholdDual nThresh = new AlgorithmThresholdDual(gImage, threshold, 1, 1, true, false);
        nThresh.run();
		
        //Only nuclei should remain, now do statistical stuff
        AlgorithmMorphology2D nFill = new AlgorithmMorphology2D(gImage, AlgorithmMorphology2D.CONNECTED4,
        		1.0f, AlgorithmMorphology2D.FILL_HOLES, 0, 0, 0, 0, true);
        nFill.run();
        
        //Count number of nuclei 
        AlgorithmMorphology2D nObj = new AlgorithmMorphology2D(gImage, AlgorithmMorphology2D.CONNECTED4,
        		1.0f, AlgorithmMorphology2D.ID_OBJECTS, 0, 0, 0, 0, true);
        nObj.setMinMax(3, 5000);
        nObj.run();
        
	}
}
