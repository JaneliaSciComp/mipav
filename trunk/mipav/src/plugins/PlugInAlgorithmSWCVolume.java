import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmThresholdDual;
import gov.nih.mipav.model.algorithms.filters.AlgorithmMean;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.HashSet;
import java.util.Iterator;
import java.util.PriorityQueue;

import WildMagic.LibFoundation.Mathematics.Vector3f;


public class PlugInAlgorithmSWCVolume extends AlgorithmBase {

	private File filFile;
	
	private ArrayList<ArrayList<float[]>> swcCoordinates;
	
	private int[] extents;
	
	private int width;
	
	private int height;
	
	private int depth;
	
	private int length;
	
	/**
	 * The following three variables are used in the radius searching portion. They tweak how fast the cutoff point is
	 * for an incomplete circle. Thus it is used to tweak for under/overshoot.
	 */

	private float sigma = 5f;
	
	private float sensitivity = 0.01f;
	
	private float radialThreshold = 0.80f;
	
	private float volume;
	
	/**
	 * For embedding the algorithm within another that already has the image open as well as the filaments processed.
	 * The most useful.
	 * 
	 * Calculates the volume of the filaments based on image intensity.
	 * 
	 * @param image
	 * @param filaments
	 */
	public PlugInAlgorithmSWCVolume(ModelImage image, ArrayList<ArrayList<float[]>> filaments){
		super(null, image);
		
		extents = srcImage.getExtents();
		width = extents[0];
		height = extents[1];
		depth = extents[2];
		length = width*height*depth;
		volume = 0f;
		
		swcCoordinates = filaments;
	}
	
	public float getVolume(){
		return volume;
	}
	
	@Override
	public void runAlgorithm() {

		BitSet mask = new BitSet(length);
		
		if(filFile != null){
			readSurfaceFile(filFile);
		}
		
		//Processing the image so that shot noise issues
		//are cleared up. Acts as a filter, although
		//edges aren't affected too adversely
		
		ModelImage cloneImage = (ModelImage)srcImage.clone();
		filterShotNoiseMean(cloneImage);
		
		AlgorithmChangeType changeZ = new AlgorithmChangeType(cloneImage, ModelImage.UBYTE,
				cloneImage.getMin(), cloneImage.getMax(), 0, 255, false);
		changeZ.run();
		
		ModelImage probImage = probabilityMap(cloneImage);
		
		float[] threshold = {0, (float) (- sensitivity * Math.log(sensitivity))};
        
        AlgorithmThresholdDual nThresh = new AlgorithmThresholdDual(probImage, threshold, 1, 1, true, false);
        nThresh.run();
       
		float[] res = srcImage.getResolutions(0);

        for(int i=0;i<swcCoordinates.size();i++){
        
			ArrayList<Vector3f> splinePts = new ArrayList<Vector3f>();
			ArrayList<float[]> fil = swcCoordinates.get(i);
			for(float[] fa : fil){
				Vector3f v = new Vector3f(fa[0], fa[1], fa[2]);
				splinePts.add(v);
			}
			
			//Use a spline to determine the gradient at a point

			PlugInAlgorithm3DSpline spline = new PlugInAlgorithm3DSpline(splinePts, res[0], res[1]);
			spline.run();
			
			ArrayList<Vector3f> xBases = spline.getXBases();
			ArrayList<Vector3f> yBases = spline.getYBases();
			ArrayList<Vector3f> zBases = spline.getZBases();
			if(i > 0){
				int connection = (int) fil.get(0)[4]-1;
				ArrayList<float[]> list = null;
				for(int k=0;k<swcCoordinates.size();k++){
					list = swcCoordinates.get(k);
					if(connection >= list.size()){
						connection -= list.size();
					}else{
						break;
					}
				}
			}
			for(int j=0;j<fil.size();j++){
			
				float[] pt0 = fil.get(j);
				
				//Gradients determined earlier are used to find the
				//plane normal to the filament so that radii can
				//be found
				ArrayList<Vector3f> rotated = spline.rotatePlane(xBases.get(j), yBases.get(j), zBases.get(j));
				
				BitSet radiusMask = calcRadius(probImage, pt0, rotated);
				
				//To prevent double counting areas, only include a point
				//if the projected radius does not intersect with another
				//point that has already been determined
				if(!radiusMask.intersects(mask)){
					pt0[7] = 1.0f;
					mask.or(radiusMask);
				}
				
			}
        }
        
        calcVolumeNew();
		
	}
	
	/**
	 * Calculates the radius at the given coordinate in the given plane
	 * which is normal to the filament.
	 * @param image
	 * @param coord
	 * @param plane
	 * @return
	 */
	private BitSet calcRadius(ModelImage image, float[] coord, ArrayList<Vector3f> plane){
		
		Vector3f center = new Vector3f(coord[0], coord[1], coord[2]);
		float[] origin = srcImage.getOrigin();
		float[] res = srcImage.getResolutions(0);
		Vector3f orgVec = new Vector3f(origin[0], origin[1], origin[2]);
		PriorityQueue<RadialElement> pq = new PriorityQueue<RadialElement>();
		
		//Translate points from plane to nearest integer coordinates
		//and place in priority queue so that you work from the 
		//center outwards
		for(int i=0;i<plane.size();i++){
			Vector3f v = plane.get(i);
			Vector3f vec = new Vector3f(v);
			vec.sub(orgVec);
			vec.add(center);
			vec.X /= res[0];
			vec.Y /= res[1];
			vec.Z /= res[2];
			int x = Math.round(vec.X);
			int y = Math.round(vec.Y);
			int z = Math.round(vec.Z);
			int ind = x + y*width + z*width*height;
			RadialElement re = new RadialElement(v, ind);
			
			if (x > 0 && x < width && y > 0 && y < height && z > 0 && z < depth)
				pq.add(re);
		}
		
		HashSet<Integer> hash = new HashSet<Integer>();
		ArrayDeque<RadialElement> queue = new ArrayDeque<RadialElement>();
		
		//Only add the closest point if multiple points have the
		//same integer coordinate
		while(!pq.isEmpty()){
			RadialElement re = pq.poll();
			Integer index = new Integer(re.ind);
			if(!hash.contains(index)){
				hash.add(index);
				queue.add(re);
			}
		}
		
		BitSet mask = new BitSet(length);
		
		float cnt = 0;
		float area = 0;
		float cutoff = res[0];
		float radius = cutoff;
		
		ArrayList<Integer> intList = new ArrayList<Integer>();
		
		/*
		 * Progress outwards from the center point and see if the point
		 * is considered foreground or background (neuron or not). Reaching
		 * a point not in the neuron will not automatically constrain the
		 * radius. A moving threshold is used to determine whether the radius
		 * has been reached to avoid spurious noise from prematurely ending
		 * the measurement. 
		 * 
		 * The basic idea is to grow a circle around the point until it 
		 * encompasses the cross section at that point. 
		 */
		while(!queue.isEmpty()){
			RadialElement re = queue.poll();
			boolean include = image.getBoolean(re.ind);
			intList.add(re.ind);
			if(include){
				cnt++;
			}
			area++;
			RadialElement next = queue.peek();
			//Automatically allow points within the smallest
			//resolution to be considered foreground
			if(re.radius < res[2]){
				mask.set(re.ind);
			}
			if(next != null){
				if(next.radius > cutoff){
					
					cutoff += res[0];
					float fraction = cnt/area;
					//Use a sigmoidal function as the moving threshold
					float threshold = radialThreshold/(1 + (float)Math.exp(-area/sigma));
					
					radius = re.radius;
					if(fraction < threshold)
						break;
					
					for(int i : intList){
						mask.set(i);
					}
					intList.clear();
				}
			}else{
				//Exhasted all points, should probably be using a larger plane
				System.err.println("Ran out of points! Need to search further next time.");
				break;
			}
		}
		
		coord[6] = radius;
		
		return mask;
		
		
	}
	
	/**
	 * Find the volume of the skeleton using the radii found in the previous methods.
	 */
	private void calcVolumeNew(){
		
		volume = 0;
		
		for(int i=0;i<swcCoordinates.size();i++){
			ArrayList<float[]> fil = swcCoordinates.get(i);

			for(int j=0;j<fil.size()-1;j++){
				float[] pt0 = fil.get(j);

				// Points are skipped if its radius would interfere with another
				// point. This prevents double counting of the volume in many
				// cases.
				if(pt0[7] == -1.0f)
					continue;

				int jp1 = j+1;
				float[] pt1 = fil.get(jp1);
				
				while(pt1[7] == -1.0f && jp1 < fil.size()-1){
					jp1++;
					pt1 = fil.get(jp1);
				}
				j=jp1-1;
				
				float dist = 0;
				for(int k=0;k<3;k++){
					float diff = pt0[k] - pt1[k];
					dist += diff*diff;
				}
				dist = (float)Math.sqrt(dist);
				float r0 = pt0[6];
				float r1 = pt1[6];
				
				//Volume is estimated by using the formula
				//for a truncated frustum, with the distance
				//between the two points as the height
				volume += dist * (r0*r0 + r0*r1 + r1*r1);
			}
			
		}
		volume *= Math.PI/3.0;
		
	}
	
	/**
	 * Method to generate a pixel probability map as part of the 
	 * pre-processing. Basically the first step in entropy 
	 * maximization calculations, each pixel is given a probability
	 * based on how likely that pixel value is in THAT image. 
	 * Due to the nature of the image, background is usually a much
	 * higher probability than signal, resulting in an image where
	 * darker pixels are more likely to be signal.
	 * 
	 * This has since been changed to output entropy:
	 * -p*ln(p)
	 * instead of just p.
	 * 
	 * @param image to transform to probability.
	 * @return the transformed probability map
	 */
	
	private ModelImage probabilityMap(ModelImage image){
		
		int value;
		ModelImage outImage;
		
		float pmax = 0f;
		int bins = 4; //Histogram bins are for 4 pixels
		float[] p = new float[256/bins];
		int[] buffer = new int[length];
		int[] histo = new int[256/bins];

		float[] pImage = new float[length];

		try {
			image.exportData(0, length, buffer);
		} catch (IOException e) {
			MipavUtil.displayError("Image locked");
			e.printStackTrace();
		}

		//accumulating pixel information
		for(int i=0;i<length;i++){
			//buffer[i] = (int)((float)buffer[i]*255f/(float)immax);
			value = buffer[i]/bins;
			histo[value]++;
		}

		for(int i=0;i<256/bins;i++){
			p[i] = (float)histo[i]/(float)length;
			if(p[i] > pmax) pmax = p[i];
		}

		float pVal;
		for(int i=0;i<length;i++){
			pVal = (p[buffer[i]/bins]);
			pImage[i] = (float) (- pVal * Math.log(pVal));
		}

		outImage = new ModelImage(ModelImage.FLOAT, extents, image.getImageName() + "_prob");
		try {
			outImage.importData(0, pImage, true);
		} catch (IOException e) {
			MipavUtil.displayError("Image locked");
			e.printStackTrace();
		}
		
		return outImage;
		
	}
	
	/**
	 * Reads surface file. Taken from drosophila registration dialog written by Nish Pandya.
	 * @param surfaceFile
	 * @return
	 */
	private boolean readSurfaceFile(File surfaceFile) {
		boolean success = true;
		RandomAccessFile raFile = null;
		try {

			raFile = new RandomAccessFile(surfaceFile, "r");
			
			String line;
			
			
			while((line=raFile.readLine())!= null) {
				line = line.trim();
				if(line.startsWith("Translate1Dragger")) {
					break;
				}
				if(line.contains("Coordinate3")) {
					ArrayList<float[]> filamentCoords = new ArrayList<float[]>();
					while(!((line=raFile.readLine()).endsWith("}"))) {
						line = line.trim();
						if(!line.equals("")) {
							if(line.startsWith("point [")) {
								line = line.substring(line.indexOf("point [") + 7, line.length()).trim();
								if(line.equals("")) {
									continue;
								}
							}
							if(line.endsWith("]")) {
								line = line.substring(0, line.indexOf("]")).trim();
								if(line.equals("")) {
									continue;
								}
							}
							if(line.endsWith(",")) {
								line = line.substring(0, line.indexOf(",")).trim();
								if(line.equals("")) {
									continue;
								}
							}
							String[] splits = line.split("\\s+");
							splits[0] = splits[0].trim();
							splits[1] = splits[1].trim();
							splits[2] = splits[2].trim();
							float coord_x = new Float(splits[0]).floatValue();
							float coord_y = new Float(splits[1]).floatValue();
							float coord_z = new Float(splits[2]).floatValue();
							  
							/**
							 * Changing from previous versions. Order is now:
							 * X, Y, Z coordinates (0, 1, 2)
							 * Distance (3)
							 * Backwards connection (4)
							 * Branch order (5)
							 * Radius (6)
							 */
							float[] coords = {coord_x,coord_y,coord_z,0,Float.NEGATIVE_INFINITY,0, -1.0f};
							
							filamentCoords.add(coords);
						}
					}
					swcCoordinates.add(filamentCoords);
				}
			}
			raFile.close();
		}catch(Exception e) {
			try {
				if(raFile != null) {
					raFile.close();
				}
			}catch(Exception ex) {
				
			}
			e.printStackTrace();
			return false;
		}
		
		return success;
	}
	
	private void filterShotNoiseMean(ModelImage image){
		int dataType = image.getType();
		int maxDiff;
		
		if(dataType == ModelImage.BYTE || dataType == ModelImage.UBYTE)
			maxDiff = 3;
		else if(dataType == ModelImage.SHORT || dataType == ModelImage.USHORT)
			maxDiff = 600;
		else return;
		
		ModelImage meanImage = (ModelImage) image.clone();
		AlgorithmMean mean = new AlgorithmMean(meanImage, 3, false, true);
		mean.run();
		int[] buffer = new int[length];
		int[] medBuffer = new int[length];
		int[] outBuffer = new int[length];
		int diff;
		
		
		try{
			image.exportData(0, length, buffer);
			meanImage.exportData(0, length, medBuffer);
		} catch(IOException e){
			MipavUtil.displayError("Could not export data from original image");
			e.printStackTrace();
		}
		
		//ArrayList<Integer> adjustPts = new ArrayList<Integer>();
		//ArrayList<Integer> addPts = new ArrayList<Integer>();
		
		HashSet<Integer> adjustPtsHash = new HashSet<Integer>();
		HashSet<Integer> addPtsHash = new HashSet<Integer>();
		
		for(int i=0;i<length;i++){
			diff = Math.abs(buffer[i] - medBuffer[i]);
			if(diff >= maxDiff){
				//adjustPts.add(i);
				buffer[i] = medBuffer[i];
				int x = i%width;
				int y = (i%(width*height))/width;
				int z = i/(width*height);
				for(int nx=x-1;nx<=x+1;nx++){
					if(nx<0 || nx>=width) continue;
					for(int ny=y-1;ny<=y+1;ny++){
						if(ny<0 || ny>=height) continue;
						for(int nz=z-1;nz<=z+1;nz++){
							if(nz<0 || nz>=depth) continue;
							int ind = nx+ny*width + nz*width*height;
							adjustPtsHash.add(ind);
						}
					}
				}
			}
		}
		
		medBuffer = null;
		
		System.arraycopy(buffer, 0, outBuffer, 0, length);
		
		while(adjustPtsHash.size()>0){
			Iterator<Integer> iter = adjustPtsHash.iterator();
			while(iter.hasNext()){
				int i = iter.next();
				int x = i%width;
				int y = (i%(width*height))/width;
				int z = i/(width*height);
				int kMed = findMean(buffer, i);
				if(Math.abs(buffer[i] - kMed) >= maxDiff){
					outBuffer[i] = kMed;
					for(int nx=x-1;nx<=x+1;nx++){
						if(nx<0 || nx>=width) continue;
						for(int ny=y-1;ny<=y+1;ny++){
							if(ny<0 || ny>=height) continue;
							for(int nz=z-1;nz<=z+1;nz++){
								if(nz<0 || nz>=depth) continue;
								int ind = nx+ny*width + nz*width*height;
								addPtsHash.add(ind);
							}
						}
					}
				}
			}
			iter = adjustPtsHash.iterator();
			while(iter.hasNext()){
				int i = iter.next();
				buffer[i] = outBuffer[i];
			}
			adjustPtsHash.clear();
			adjustPtsHash.addAll(addPtsHash);
			addPtsHash.clear();
		}
		
		meanImage.disposeLocal();
		try {
			image.importData(0, outBuffer, true);
		} catch (IOException e) {
			MipavUtil.displayError("Unable to import filtered image");
			e.printStackTrace();
		}
		
	}

	private int findMean(int[] buffer, int i){
		int x = i%width;
		int y = (i%(width*height))/width;
		int z = i/(width*height);
		int kWidth = Math.min(3, 2 + Math.min(x, width-1-x));
		int kHeight = Math.min(3, 2 + Math.min(y, height-1-y));
		int kDepth = Math.min(3, 2 + Math.min(z, depth-1-z));
		int cnt = kWidth*kHeight*kDepth;
		int sum = 0;
	
		for(int nx=x-1;nx<=x+1;nx++){
			if(nx<0 || nx>=width) continue;
			for(int ny=y-1;ny<=y+1;ny++){
				if(ny<0 || ny>=height) continue;
				for(int nz=z-1;nz<=z+1;nz++){
					if(nz<0 || nz>=depth) continue;
					sum += buffer[nx+ny*width + nz*width*height];
				}
			}
		}
		int kMean = (int) ((float)sum / (float)cnt);
		return kMean;
	}

	private class RadialElement implements Comparable<RadialElement> {
		
		private float radius;
		
		private int ind;
		
		private RadialElement(Vector3f point, int index) {
			radius = point.distance(new Vector3f());
			ind = index;
		}
		
		@Override
		public int compareTo(RadialElement o) {
			float ro = o.radius;
			if(radius > ro)
				return 1;
			else if(radius < ro)
				return -1;
			return 0;
		}
		
	}

}
