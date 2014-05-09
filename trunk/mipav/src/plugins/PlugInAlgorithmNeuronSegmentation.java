import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.HashSet;
import java.util.Iterator;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmMorphology2D;
import gov.nih.mipav.model.algorithms.AlgorithmThresholdDual;
import gov.nih.mipav.model.algorithms.filters.AlgorithmMean;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageCalculator;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;

/**
 * Algorithm for neuron segmentation used to trace branches of the neuron
 * for the Giniger lab. The algorithm first segments the neuron using a
 * series of preprocessing steps + a threshold, and then skeletonized
 * to reveal the branches. 
 * 
 * The algorithm also contains methods to add or delete branches from
 * the skeletonized segmentation, which are called from the dialog
 * as the user clicks on the image. 
 * 
 * @author wangvg
 *
 */

public class PlugInAlgorithmNeuronSegmentation extends AlgorithmBase {
	
	/**
	 * Coordinates of the growth cone centroid, first 
	 * calculated using the branch tips, and then by
	 * the polygonal area
	 */
	private float[] centroidPts;

	/**
	 * The index in the VOI list that the centroid
	 * VOI resides. Is -1 if it is not displayed.
	 */
	private int centroidVOI;
	
	private int chooseX = -1;
	
	private int chooseY = -1;
	
	/**
	 * The VOI list to be displayed by the image.
	 */
	private VOIVector displayed;
	
	/**
	 * Contains the ArrayList index of the tipPts that
	 * constitute the longest path in the skeleton.
	 */
	private int[] endIndex;
	
	private int[] extents;
	
	private int height;
	
	private int[] imBuffer;
	
	private int length;
	
	private int neuronArea;

	/**
	 * The polygonal area as determined by the convex hull VOI
	 */
	private int polyArea;
	
	/**
	 * The index in the VOI list that the polygonal
	 * area VOI resides. Is -1 if it is not displayed.
	 */
	private int polygonalVOI;
	
	/**
	 * An image for pixel probabilities in the original
	 * source image. Used as a cost function for the
	 * psuedo-Dijkstra routine
	 */
	private ModelImage probImage;
	
	private ModelImage segImage;
	/**
	 * Used after the initial segmentation to change the
	 * upper threshold on the probability map segmentation
	 * 
	 * Default sensitivity is 0.01
	 */
	private float sensitivity = 0.01f;
	/**
	 * The bitset containing the skeletonized segmentation 
	 * of the neuron, which ideally traces the branches
	 * of the structure
	 */
	private BitSet skeleton;

	/**
	 * The next index to put the VOI in. Should be equivalent
	 * to the vector size.
	 */
	private int slot;
	
	/**
	 * Output stream used to write the necessary information
	 * for the SWC format. 
	 */
	
	private FileWriter swcOut;
	
	/**
	 * List of integer indecies that constitute the tips
	 * of branches. This can grow and shrink as branches
	 * are added and deleted by the user. 
	 */
	private ArrayList<Integer> tipPts;

	/**
	 * The index in the VOI list that the branch
	 * tips VOI resides. Is -1 if it is not displayed.
	 */
	private int tipsVOI;

	/**
	 * There are thinkers and there are doers. This, however, is
	 * an undoer which is a container used to provide rudimentary
	 * undo/redo functionality by storing the previous state during
	 * modifications.
	 */
	private UndoContainer undoer;

	/**
	 * The displayed polygonal area VOI, which results from a
	 * convex hull operation.
	 */
	private VOI voiHull;
	
	private int width;
	
	public PlugInAlgorithmNeuronSegmentation(ModelImage source){
		
		super(null, source);
		
		extents = source.getExtents();
		width = extents[0];
		height = extents[1];
		length = width*height;
		centroidPts = new float[2];
		
		imBuffer = new int[length];
		
		try {
			source.exportData(0, length, imBuffer);
		} catch (IOException e) {
			MipavUtil.displayError("Image locked");
			e.printStackTrace();
		}

		endIndex = new int[2];

	}
	
	/**
	 * Method, as called by the accompanying dialog, that adds branches
	 * to the skeleton representation of the neuron. Uses a modified
	 * Dijkstra's Method to determine the shortest path from the target
	 * to the skeleton.
	 * 
	 * @param target : index of the start location for the Dijkstra's 
	 * algorithm
	 */
	
	public void addBranches(int target){
		
		//For undoing this modification if need be
		undoer.update();
		
		//See documentation in algorithm if details are needed
		//It is generally just Dijkstra's algorithm though
		PlugInAlgorithmDijkstraCode add = new PlugInAlgorithmDijkstraCode(probImage, skeleton, target, 0);
		add.run();
		
		skeleton = add.getNewSkeleton();
		
		//Need to remove any previously registered tips if the new 
		//branch reaches one. This is done so that the list does
		//not need to be repopulated every time
		BitSet path = add.getShortestPath();
		for(int i = path.nextSetBit(0); i>=0; i=path.nextSetBit(i+1)){
			int x = i%width;
			int y = i/width;
			for(int ny = y-1;ny<=y+1;ny++){
				if(ny < 0 || ny >= height) continue;
				for(int nx = x-1;nx<=x+1;nx++){
					if(nx < 0 || nx >= width) continue;
					if(tipPts.contains(new Integer(i)))
						tipPts.remove(new Integer(i));
				}
			}
		}
		
		//Add the new point and recalculate the polygonal area/centroid
		tipPts.add(new Integer(target));
		polygonalArea();
		
	}
	
	public void cleanImages(){
		segImage.disposeLocal();
		probImage.disposeLocal();
	}
	
	/**
	 * Method, as called by the accompanying dialog, that deletes branches
	 * from the skeleton representation of the neuron. From the seed point, 
	 * all pixels are removed until an intersection with another branch
	 * occurs, in which case the deletions are terminated.
	 * 
	 * @param target seed point for the deletion. Propagates in both 
	 * directions along the branch
	 */
	
	public void deleteBranches(int target){
		
		//The queue to hold the pixels for deletion
		ArrayList<Integer> pixels = new ArrayList<Integer>();
		ArrayList<Integer> check = new ArrayList<Integer>();
		int num;
		int nx;
		int ny;
		int ind;
		int x = target % width;
		int y = target / width;

		//If target is not on the skeleton, search within a 3x3
		//box around the pixel for the nearest pixel, and reseed
		if(!skeleton.get(target)){
			for(int i=-1;i<=1;i++){
				ny = y+i;
				for(int j=-1;j<=1;j++){
					nx = x+j;
					ind = nx + ny*width;
					if(skeleton.get(ind)){
						target = ind;
					}
				}
			}
		}
		
		//If the above fails, then simply return without doing
		//anything before undo storage is updated
		if(!skeleton.get(target)) return;

		//Store last modification for undo purposes
		undoer.update();
		
		//Delete the seed point, and add all neighboring true
		//pixels for deletion (8 connected)
		skeleton.flip(target);
		tipPts.remove(new Integer(target));
		x = target%width;
		y = target/width;
		for(int i=-1;i<=1;i++){
			ny = y+i;
			for(int j=-1;j<=1;j++){
				nx = x+j;
				ind = nx + ny*width;
				if(skeleton.get(ind)){
					pixels.add(new Integer(ind));
				}
			}
		}
		
		//Delete each pixel added to the queue, and adds the
		//next pixel scheduled for deletion. However, if a
		//pixel spaws more than one new pixel for deletion
		//(a.k.a an intersection with another branch), then
		//remove those pixels that were just added to the
		//deletion queue
		
		while(!pixels.isEmpty()){
			num = 0;
			target = pixels.remove(0).intValue();
			skeleton.flip(target);
			x = target%width;
			y = target/width;
			for(int i=-1;i<=1;i++){
				ny = y+i;
				for(int j=-1;j<=1;j++){
					nx = x+j;
					ind = nx + ny*width;
					if(skeleton.get(ind)){
						num++;
						pixels.add(new Integer(ind));
					}
				}
			}
			
			//Check to see if removing this pixel causes a
			//discontinuity in the skeleton by checking 
			//all pixel combinations
			if(num>2){
				for(int i=0;i<num;i++){
					check.add(pixels.remove(pixels.size()-1));
				}
				int numDiff = 0;
				for(int i=0;i<num;i++){
					for(int j=i+1;j<num;j++){
						int x1 = check.get(i)%width;
						int y1 = check.get(i)/width;
						int x2 = check.get(j)%width;
						int y2 = check.get(j)/width;
						if((Math.abs(x1-x2)==2 || Math.abs(y1-y2)==2))
							numDiff++;
					}
				}
				if(numDiff>num-2)
					skeleton.set(target);
			} else if(num==2){
				for(int i=0;i<num;i++){
					check.add(pixels.remove(pixels.size()-1));
				}
				
				int x1 = check.get(0)%width;
				int y1 = check.get(0)/width;
				int x2 = check.get(1)%width;
				int y2 = check.get(1)/width;
				if((Math.abs(x1-x2)==2 || Math.abs(y1-y2)==2))
					skeleton.set(target);
			}
			else if(num==0){
				//Once you have reached the tip, remove it from the
				//list of branch tips (since it is no longer present)
				tipPts.remove(new Integer(target));
			}
		}
		polygonalArea();
	}
	
	/**
	 * Display method used to show the centroid VOI. The point VOI
	 * is created from the calculated centroid points and then
	 * added to the vector of displayed VOIs. If this VOI is already
	 * displayed (for when modifications are made to the skeleton),
	 * you need to also update the rest of the list.
	 * 
	 * Mostly used for debugging
	 */
	
	public void displayCentroid(){
		
		if(centroidVOI != -1){
			displayed.remove(centroidVOI);
			if(tipsVOI > centroidVOI) tipsVOI--;
			if(polygonalVOI > centroidVOI) polygonalVOI--;
			slot--;
		}
		
		VOI voi = new VOI((short) 1, "Centroid", VOI.POINT, -1);
		voi.importPoint(new Vector3f(centroidPts[0], centroidPts[1], 0));
		displayed.add(voi);
		srcImage.setVOIs(displayed);
		srcImage.notifyImageDisplayListeners(null, true, 0, -1);
		
		centroidVOI = slot;
		slot++;
		
	}
	
	/**
	 * Display method used to show the polygonal area VOI. The contour 
	 * VOI is taken from the results of the convex hull and then
	 * added to the vector of displayed VOIs. If this VOI is already
	 * displayed (for when modifications are made to the skeleton),
	 * you need to also update the rest of the list.
	 */
	
	public void displayPolygonal(){
				
		if(polygonalVOI != -1){
			displayed.remove(polygonalVOI);
			if(tipsVOI > polygonalVOI) tipsVOI--;
			if(centroidVOI > polygonalVOI) centroidVOI--;
			slot--;
		}
		
		displayed.add(voiHull);
		polygonalVOI = slot;
		slot++;
		srcImage.setVOIs(displayed);
		srcImage.notifyImageDisplayListeners(null, true, 0, -1);
		
	}
	
	/**
	 * Display method used to show the centroid VOI. The multi-point 
	 * VOI is created from the calculated tip points and then
	 * added to the vector of displayed VOIs. If this VOI is already
	 * displayed (for when modifications are made to the skeleton),
	 * you need to also update the rest of the list.
	 * 
	 * Mostly used for debugging
	 */
	
	public void displayTips(){
		
		int x,y,ind;

		if(tipsVOI != -1) {
			displayed.remove(tipsVOI);
			slot--;
			if(centroidVOI > tipsVOI) centroidVOI--;
			if(polygonalVOI > tipsVOI) polygonalVOI--;
		}
		VOI voi = new VOI((short) 0, "Endpoints", VOI.POINT, 0);
		for(int i=0;i<tipPts.size();i++){
			ind = tipPts.get(i);
			x = ind%width;
			y = ind/width;
			voi.importPoint(new Vector3f(x,y,0));
		}
		
		displayed.add(voi);
		srcImage.setVOIs(displayed);
		srcImage.notifyImageDisplayListeners(null, true, 0, -1);
		
		tipsVOI = slot;
		slot++;
		
	}
	
	public void finalize(){
		probImage.disposeLocal();
		segImage.disposeLocal();
		skeleton = null;
		undoer = null;
		
		super.finalize();
	}
	
	public float[] getCentroid(){
		return centroidPts;
	}
	
	public ModelImage getSegImage(){
		ModelImage segClone = (ModelImage) segImage.clone();
		segClone.setImageName(srcImage.getImageName() + "_segmentation");
		return segClone;
	}
	
	public ArrayList<Integer> getTipPts(){
		return tipPts;
	}
	
	public int[] getChosenSpot(){
		return new int[]{chooseX, chooseY};
	}
	
	public int getPolyArea(){
		return polyArea;
	}
	
	/**
	 * Retrieves the skeleton as a BitSet, which can then be displayed
	 * in the dialog as a paint mask (was originally another image, but
	 * this takes up less memory and works about the same)
	 * 
	 * @return the BitSet representation of the neuron branches
	 */
	
	public BitSet getSkeleton(){
		return skeleton;
	}
	
	/**
	 * Display method used to stop showing the centroid VOI. The VOI is
	 * removed from the image's VOI list, and VOI indecies are updated
	 * to reflect that change.  
	 */
	
	public void removeCentroid(){
		
		displayed.remove(centroidVOI);
		if(tipsVOI > centroidVOI) tipsVOI--;
		if(polygonalVOI > centroidVOI) polygonalVOI--;
		slot--;
		centroidVOI = -1;
		srcImage.setVOIs(displayed);
		srcImage.notifyImageDisplayListeners(null, true, 0, -1);
		
	}
	
	/**
	 * Display method used to stop showing the polygonal area VOI. 
	 * The VOI is removed from the image's VOI list, and VOI 
	 * indecies are updated to reflect that change.  
	 */
	
	public void removePolygonal(){
		
		displayed.remove(polygonalVOI);
		if(tipsVOI > polygonalVOI) tipsVOI--;
		if(centroidVOI > polygonalVOI) centroidVOI--;
		slot--;
		polygonalVOI = -1;
		srcImage.setVOIs(displayed);
		srcImage.notifyImageDisplayListeners(null, true, 0, -1);
		
	}
	
	/**
	 * Display method used to stop showing the tips VOI. The VOI is
	 * removed from the image's VOI list, and VOI indecies are updated
	 * to reflect that change.  
	 */
	
	public void removeTips(){
		
		displayed.remove(tipsVOI);
		if(centroidVOI > tipsVOI) centroidVOI--;
		if(polygonalVOI > tipsVOI) polygonalVOI--;
		slot--;
		tipsVOI = -1;
		srcImage.setVOIs(displayed);
		srcImage.notifyImageDisplayListeners(null, true, 0, -1);
		
	}
	
	/**
	 * Processing step to get the skeletonized segmentation of the
	 * neuron. Is initially run at the beginning of the dialog
	 * so that the user is presented with the initial skeleton to
	 * edit.
	 * 
	 * (non-Javadoc)
	 * @see gov.nih.mipav.model.algorithms.AlgorithmBase#runAlgorithm()
	 */

	@Override
	public void runAlgorithm() {
		
		//Initialize some variables here, as runAlgorithm may be run
		//multiple times, as it is connected to the dialog's slider.
		//New runs will reset these variables, which is desired.
		skeleton = new BitSet(length);
		displayed = srcImage.getVOIs();
		displayed.clear();
		slot = 0;
		tipPts = new ArrayList<Integer>();
		
		centroidVOI = -1;
		tipsVOI = -1;
		polygonalVOI = -1;

		undoer = new UndoContainer();
		
		if(probImage != null){
			probImage.disposeLocal();
		}
		if(segImage != null){
			segImage.disposeLocal();
		}

		//Pre-processing step for segmentation. 
		//See method for details on what it does.
		filterShotNoiseMean(srcImage);
		ModelImage zImage = (ModelImage)srcImage.clone();
		
		fireProgressStateChanged(25);
		
		AlgorithmChangeType changeZ = new AlgorithmChangeType(zImage, ModelImage.UBYTE,
				zImage.getMin(), zImage.getMax(), 0, 255, false);
		changeZ.run();
		destImage = probabilityMap(zImage);

		//Store the cost function for the modified 
		//Dijkstra's method, and convert to UBYTE
		probImage = (ModelImage) destImage.clone();
		probImage.setImageName(srcImage.getImageName() + "_entropy");
		AlgorithmChangeType change = new AlgorithmChangeType(probImage, ModelImage.UBYTE,
				probImage.getMin(), probImage.getMax(), 0, 255,  false);
		change.run();
		
		fireProgressStateChanged(45);
		
		//Hard segmentation of the filtered image
		//results in a general structure for the
		//growth cone, which can then be skeletonized
		//to identify branches.
		
		float[] threshold = {0, (float) (- sensitivity * Math.log(sensitivity))};
        
        AlgorithmThresholdDual nThresh = new AlgorithmThresholdDual(destImage, threshold, 1, 1, true, false);
        nThresh.run();
        
        largestObject();
        
        fireProgressStateChanged(90);
        
        AlgorithmMorphology2D skeletonize = new AlgorithmMorphology2D(destImage, AlgorithmMorphology2D.CONNECTED4,
        		1.0f, AlgorithmMorphology2D.SKELETONIZE, 0, 0, 0, 0, true);
        skeletonize.run();
        
        try {
			destImage.exportData(0, length, skeleton);
		} catch (IOException e) {
			MipavUtil.displayError("Image locked");
			e.printStackTrace();
		}
        
        //Calculate initial VOIs, which can then be displayed later
        
        findTips();
        polygonalArea();
        
        if(chooseX == -1 || chooseY == -1){
        	chooseX = (int) centroidPts[0];
        	chooseY = (int) centroidPts[1];
        }
        
        fireProgressStateChanged(100);
        
        destImage.disposeLocal();
        zImage.disposeLocal();
	}
	
	/**
	 * Method to save the neuron branches skeleton as well as the VOIs
	 * that describe the growth cone. Skeleton images are saved in a
	 * subfolder of the original image, labeled "Branch_Images." Each
	 * skeleton image then has its own subfolder to save the VOIs, 
	 * labeled "Endpoints", "Centroid", and "Polygonal Area."
	 * 
	 * VOIs can be saved, an option was added to the dialog
	 * that allows for the toggling
	 */
	
	public void save(boolean saveVOI, boolean saveSkel){
		
		int ind, x,y;

		String imDir = srcImage.getImageDirectory() + File.separator + "Branch_Images" + File.separator;
		File dirFile = new File(imDir);
		if(!dirFile.exists())
			dirFile.mkdir();
		
		saveAsSWC(); 
		
		ModelImage skelImage = new ModelImage(ModelImage.USHORT, extents, srcImage.getImageName().concat("_branches"));
		int[] skelBuffer = new int[length];
		for(int i=skeleton.nextSetBit(0);i>0;i=skeleton.nextSetBit(i+1)){
			skelBuffer[i] = 255;
		}
		try {
			skelImage.importData(0, skelBuffer, true);
		} catch (IOException e1) {
			MipavUtil.displayError("Image locked");
			e1.printStackTrace();
		}
		
		if(saveSkel){
			skelImage.saveImage(imDir, null, FileUtility.TIFF, true);	
		}
		
		if(saveVOI){
			
			String voiDir = imDir + File.separator + "defaultVOIs_" + skelImage.getImageName() + File.separator;
			dirFile = new File(voiDir);
			if(!dirFile.exists())
				dirFile.mkdir();
			FileVOI saver;
	
			VOIVector copy = new VOIVector();
			copy.add(voiHull);
			VOI voi = new VOI((short) 1, "Centroid", VOI.POINT, -1);
			voi.importPoint(new Vector3f(centroidPts[0], centroidPts[1], 0));
			copy.add(voi);
			voi = new VOI((short) 0, "Endpoints", VOI.POINT, 0);
			for(int i=0;i<tipPts.size();i++){
				ind = tipPts.get(i);
				x = ind%width;
				y = ind/width;
				voi.importPoint(new Vector3f(x,y,0));
			}
			copy.add(voi);
			try {
				for(int i=0;i<3;i++){
					saver = new FileVOI(copy.VOIAt(i).getName() + ".xml", voiDir, skelImage);
		            saver.writeVOI(copy.VOIAt(i), true);
				}
			} catch (IOException e) {
				MipavUtil.displayError("VOI file locked");
				e.printStackTrace();
			}
		}

		skelImage.disposeLocal();
	}
	
	/**
	 * Redone version, a little more robust. It has fewer problems
	 * making the right connections when viewed in the SWC format
	 * 
	 * Method for saving the skeletonized neuron in the SWC format. By
	 * using the coordinates gathered from the tips as 
	 * determined by the skeleton, traverse the skeleton and add 
	 * information to the text file (using writeInfo) whenever you hit
	 * one of these points.
	 * 
	 * Start at one of the two endpoints as determined by the longest
	 * path in the skeleton.
	 * 
	 * This method can handle stranger structures, especially those
	 * with loops in them
	 */
	
	public void saveAsSWC(){
		longestPath(); //determine the endpoints of the longest path
		polygonalArea(); //Update polygonal area and centroid location
		findTips(); //Update tip points, hasn't been completely accurate
		
		int x,y,ind, num;
		BitSet skelClone = (BitSet)skeleton.clone();
		
		//Used as a stack to follow paths to completion
		ArrayDeque<BranchContainer> path = new ArrayDeque<BranchContainer>();
		//List of points to add to the stacks after searching
		ArrayList<Integer> pathBuffer = new ArrayList<Integer>();
		
		int start = tipPts.get(endIndex[0]);
		int line = 1;

		BranchContainer current;
		
		String swcDir = srcImage.getImageDirectory() + File.separator + "Branch_Images" + File.separator;
		swcDir += srcImage.getImageName().concat("_branches.swc");
		//String areaStr = "# Polygonal Area: " + String.valueOf(polyArea) + "\n";
		//String centroidStr = "# Centroid: (" + String.valueOf(centroidPts[0]) + ", "
		//		+ String.valueOf(centroidPts[1]) + ")\n";
		String dimStr = String.format("# Width %d\n# Height %d\n", width, height);
		String noteStr = 
				  "########################################################\n"
				+ "#              START OF SWC COORDINATES                #\n"
				+ "########################################################\n"
				+ "# NOTE: The above coordinates are in image space coordinates\n"
				+ "# Y-coordinates for SWC format are inverted in image space.\n"
				+ "# Image Y-coordinate = Image Height - SWC Y-coordinate\n";
		
		try {
			swcOut = new FileWriter(swcDir);
			//swcOut.append(areaStr);
			//swcOut.append(centroidStr);
			swcOut.append(dimStr);
			swcOut.append(noteStr);
			swcOut.flush();
		} catch (IOException e) {
			MipavUtil.displayError("Unable to export to SWC file");
			return;
		}

		//Delete the trail behind you as you traverse the skeleton
		skelClone.flip(start);
		
		//Start seeding traversal with one of the longest path points
		x = start%width;
		y = start/width;
		for(int ny=y+1;ny>=y-1;ny--){
			if(ny<0||ny>=height) continue;
			for(int nx=x+1;nx>=x-1;nx--){
				if(nx<0||nx>=width) continue;
				ind = nx + ny*width;
				if(skelClone.get(ind)){
					path.addFirst(new BranchContainer(ind, 1, start));
					skelClone.flip(ind);
				}
			}
		}
		
		writeInfo(1, tipPts.get(endIndex[0]), -1, -1);
		
		while(!path.isEmpty()){
			num = 0;
			current = path.pop();
			start = current.i;
			x = start%width;
			y = start/width;
			int originated = current.originated;
			for(int ny=y+1;ny>=y-1;ny--){
				if(ny<0||ny>=height) continue;
				for(int nx=x+1;nx>=x-1;nx--){
					if(nx<0||nx>=width || (nx == x && ny == y)) continue;
					ind = nx + ny*width;
					if(skelClone.get(ind)){
						//Reached the end of a branch, don't need to add
						//anymore points, just write information
						if(tipPts.contains(ind)){
							line++;
							writeInfo(line, ind, 1, current.line);
						}
						else{
							pathBuffer.add(ind);
							num++;
						}
					}
				}
			}
			//Refill the stack with points to traverse
			if(num >= 2){
				line++;
				writeInfo(line, start, 0, current.line);
				current.line = line;
				originated = start;
			}
			//Check if you need to bridge any gaps in cyclic neurons
			else if(num == 0){
				Iterator<BranchContainer> iter;
				BranchContainer check;
				BranchContainer toRemove = null;
				loop:for(int ny=y+1;ny>=y-1;ny--){
					if(ny<0||ny>=height) continue;
					for(int nx=x+1;nx>=x-1;nx--){
						if(nx<0||nx>=width || (nx == x && ny == y)) continue;
						ind = nx + ny*width;
						iter = path.iterator();
						while(iter.hasNext()){
							check = iter.next();
							if(check.i == ind && check.line != current.line){
								line++;
								writeInfo(line, current.originated, 0, check.line);
								line++;
								writeInfo(line, check.originated, 0, current.line);
								toRemove = check;
								break loop;
							}
						}
					}
				}
				if(toRemove != null) path.remove(toRemove);
			}
			while(!pathBuffer.isEmpty()){
				path.addFirst(new BranchContainer(pathBuffer.get(0), current.line, originated));
				skelClone.flip(pathBuffer.remove(0));
			}
		}
		
		try {
			swcOut.close();
		} catch (IOException e) {
			MipavUtil.displayError("Unable to close connection to SWC file");
			return;
		}
	}
	
	public void setCoords(int x, int y){
		chooseX = x;
		chooseY = y;
	}
	
	/**
	 * Changes the initial segmentation sensitivity to add or
	 * remove branches from the skeleton. 
	 * @param s threshold paramter for initial segmentation
	 */
	
	public void setSensitivity(float s){
		sensitivity = s;
	}
	
	/**
	 * Method to undo/redo the last modification made to
	 * the skeleton. All modifications are stored in a 
	 * container class, <code>UndoContainer</code> for
	 * easy storage and recall. However, it only provides
	 * one level of undo/redo.
	 * 
	 */
	
	public void undo(){
	
		undoer.recall();
	
	}
	
	/**
	 * Not in use right now. It is wrong and requires some edits before
	 * it is useable
	 */
	
	private void calcArea(){
		
		ModelImage areaIm = (ModelImage)segImage.clone();
		
		AlgorithmMorphology2D nObj = new AlgorithmMorphology2D(areaIm, AlgorithmMorphology2D.CONNECTED4,
        		1.0f, AlgorithmMorphology2D.ID_OBJECTS, 0, 0, 0, 0, true);
        nObj.setMinMax(1, length);
        nObj.run();
        
        int[] buffer = new int[length];
        
        try {
			areaIm.exportData(0, length, buffer);
		} catch (IOException e) {
			MipavUtil.displayError("Image locked");
			e.printStackTrace();
		}
        
        HashSet<Integer> areaList = new HashSet<Integer>();
        int value;
        
        for(int i=skeleton.nextSetBit(0);i>0;i=skeleton.nextSetBit(i+1)){
        	value = buffer[i];
        	if(value != 0)
        		areaList.add(value);
        }

        neuronArea = 0;
		for(int i=0;i<length;i++){
			value = buffer[i];
			if(value != 0 && areaList.contains(value)){
				neuronArea++;
			}
		}
	}
	
	/**
	 * Method to find any ends of branches. All the true bits in the skeleton
	 * bit set are tested for the number of neighbors. Only bits with one true
	 * neighbor is considered a valid tip. A preliminary centroid is also
	 * determined from the tips which helps with the polygonal area calculation,
	 * which occurs afterwards.
	 */
	
	private void findTips(){
		
		int x, y, nx, ny, nind, num;
		/*float area = 0;
		float sumX = 0;
		float sumY = 0;*/
		for (int ind = skeleton.nextSetBit(0); ind >= 0; ind = skeleton.nextSetBit(ind+1)) {
			num=0;
			x = ind%width;
			y = ind/width;
			for(int i=-1;i<=1;i++){
				ny = y+i;
				if(ny < 0 || ny >= height) continue;
				for(int j=-1;j<=1;j++){
					nx = x+j;
					if(nx < 0 || nx >= width) continue;
					nind = nx + ny*width;
					if(skeleton.get(nind) && nind != ind) num++;
				}
			}
			if(num==1) {
				tipPts.add(new Integer(ind));
				/*sumX += x;
				sumY += y;
				area++;*/
			}
		}
		
		/*centroidPts = new float[2];
		centroidPts[0] = sumX/area;
		centroidPts[1] = sumY/area;*/
		
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
		AlgorithmMean mean = new AlgorithmMean(meanImage, 3, true);
		mean.run();
		int length = width*height;
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
		
		ArrayList<Integer> adjustPts = new ArrayList<Integer>();
		ArrayList<Integer> addPts = new ArrayList<Integer>();
		for(int i=0;i<length;i++){
			diff = Math.abs(buffer[i] - medBuffer[i]);
			if(diff >= maxDiff){
				//adjustPts.add(i);
				buffer[i] = medBuffer[i];
				int x = i%width;
				int y = i/width;
				for(int nx=x-1;nx<=x+1;nx++){
					if(nx<0 || nx>=width) continue;
					for(int ny=y-1;ny<=y+1;ny++){
						if(ny<0 || ny>=height) continue;
						int ind = nx+ny*width;
						if(!adjustPts.contains(ind))
							adjustPts.add(nx+ny*width);
					}
				}
			}
		}
		
		medBuffer = null;
		
		System.arraycopy(buffer, 0, outBuffer, 0, length);
		
		while(adjustPts.size()>0){
			int size = adjustPts.size();
			for(int j = 0;j<size;j++){
				int i = adjustPts.get(j);
				int x = i%width;
				int y = i/width;
				int kMed = findMean(buffer, i);
				if(Math.abs(buffer[i] - kMed) >= maxDiff){
					outBuffer[i] = kMed;
					//adjustPts.add(i);
					for(int nx=x-1;nx<=x+1;nx++){
						if(nx<0 || nx>=width) continue;
						for(int ny=y-1;ny<=y+1;ny++){
							if(ny<0 || ny>=height) continue;
							int ind = nx+ny*width;
							if(!addPts.contains(ind))
								addPts.add(nx+ny*width);
						}
					}
				}
			}
			for(int j = 0;j<size;j++){
				int i=adjustPts.remove(0);
				buffer[i] = outBuffer[i];
			}
			adjustPts.addAll(addPts);
			addPts.clear();
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
		int y = i/width;
		int kWidth = Math.min(3, 2 + Math.min(x, width-1-x));
		int kHeight = Math.min(3, 2 + Math.min(y, height-1-y));
		int cnt = kWidth*kHeight;
		int sum = 0;
	
		for(int nx=x-1;nx<=x+1;nx++){
			if(nx<0 || nx>=width) continue;
			for(int ny=y-1;ny<=y+1;ny++){
				if(ny<0 || ny>=height) continue;
				sum += buffer[nx+ny*width];
			}
		}
		int kMean = (int) ((float)sum / (float)cnt);
		return kMean;
	}
	
	/**
	 * Method as part of segmentation process. It searches
	 * for the largest object in the initial segmentation
	 * to pick out (what should be) the growth cone. However,
	 * as opposed to just area, it searchers for the object
	 * with the highest overall intensity which ideally should
	 * be the neuron.
	 */
	
	private void largestObject(){
		
		float dist, adjusted;
		int immax;
		int x,y,diffX,diffY;
        int histmax = 0;
        int ind = 0;
        int[] histo; 
		int[] buffer = new int[length];
        
		//Close any small holes (less than area of 15 pixels) in segementation
		//That can cause weird small loops in the skeleton
        ModelImage inverseImage = (ModelImage) destImage.clone();
        AlgorithmChangeType invert = new AlgorithmChangeType(inverseImage, ModelImage.BOOLEAN, 0.0, 1.0, 1.0, 0.0, true);
        invert.run();
        
        AlgorithmMorphology2D holes = new AlgorithmMorphology2D(inverseImage, AlgorithmMorphology2D.CONNECTED4,
        		1.0f, AlgorithmMorphology2D.ID_OBJECTS, 0, 0, 0, 0, true);
        holes.setMinMax(1, 15);
        holes.run();
        
        AlgorithmThresholdDual holeThresh = new AlgorithmThresholdDual(inverseImage, 
        		new float[]{1, (float) inverseImage.getMax()}, 1, 1, true, false);
        holeThresh.run();
        
        AlgorithmImageCalculator imCalc = new AlgorithmImageCalculator(destImage, inverseImage, 
        		AlgorithmImageCalculator.OR, AlgorithmImageCalculator.CLIP, true, "");
        imCalc.run();
        
        inverseImage.disposeLocal();
        segImage = (ModelImage) destImage.clone();
        segImage.setImageName(srcImage.getImageName() + "_segstored");
        //End close small holes
        
        fireProgressStateChanged(70);
        
        //Count/distinguish the objects in the image
		AlgorithmMorphology2D nObj = new AlgorithmMorphology2D(destImage, AlgorithmMorphology2D.CONNECTED4,
        		1.0f, AlgorithmMorphology2D.ID_OBJECTS, 0, 0, 0, 0, true);
        nObj.setMinMax(2, length);
        nObj.run();
        
        immax = (int) destImage.getMax();
        histo = new int[immax+1];
        
        try {
			destImage.exportData(0, length, buffer);
		} catch (IOException e) {
			MipavUtil.displayError("Image locked");
			e.printStackTrace();
		}
        
        //Accumulate area information based on
        //output of object counting

        for(int i=0;i<length;i++){
        	if(buffer[i] != 0){
        		//Use an adjusted measure to take into account distance
        		//from chosen point
        		if(chooseX >= 0 && chooseY >= 0){
        			x = i%width;
        			y = i/width;
        			diffX = x - chooseX;
        			diffY = y - chooseY;
        			dist = (float) Math.sqrt(diffX*diffX + diffY*diffY);
        			adjusted = (float) (10*imBuffer[i]) / dist;
        			histo[buffer[i]] += adjusted;
        		}
        		else
        			histo[buffer[i]]+=imBuffer[i];
        	}
        }
        
        //Determine which pixel value contains
        //the largest object in the image

        for(int i=1;i<immax+1;i++){
        	if(histo[i] > histmax) {
        		histmax = histo[i];
        		ind = i;
        	}
        }
        
        //Pick only the pixel which contains
        //the largest object, and that should
        //be the growth cone
        float[] threshold = {ind, ind};
        
        AlgorithmThresholdDual nThresh = new AlgorithmThresholdDual(destImage, threshold, 1, 1, true, false);
        nThresh.run();

	}
	
	/**
	 * Method used to determine which endpoints (from the tipPts list) constitute
	 * the longest path of the skeleton. Goes through every possible combination of
	 * paths and traverses the skeleton to determine the length between two endpoints.
	 * The endpoints are stored as the index number of the point in tipPts for recall.
	 * Is used to start seeding for the SWC file. 
	 * 
	 * There is a slightly faster way to do this that can be added in later, you only
	 * do N-loops instead of NxM, and then just full traverse the skeleton each time
	 * instead of partially traversing the skeleton, and keep track of it that way. 
	 * Probably slightly faster, but the speedup might be negligeable for an application 
	 * like this.
	 */
	
	private void longestPath(){

		int startInd;
		int endInd;
		int longest = 0;
		int x,y,ind, length;
		
		PathLength current;
		
		BitSet skelClone; 
		ArrayList<PathLength> queue = new ArrayList<PathLength>();
		
		for(int i=0;i<tipPts.size()-1;i++){
			for(int j=i+1;j<tipPts.size();j++){
				queue.clear();
				skelClone = (BitSet) skeleton.clone();
				startInd = tipPts.get(i);
				endInd = tipPts.get(j);
				skelClone.flip(startInd);
				x = startInd%width;
				y = startInd/width;
				
				//Start seeding at one of the endpoints
				for(int nx=x-1;nx<=x+1;nx++){
					if (nx<0 || nx>=width) continue;
					for(int ny=y-1;ny<=y+1;ny++){
						if(ny<0 || ny>=height) continue;
						ind = nx + ny*width;
						if(skelClone.get(ind))
						{
							queue.add(new PathLength(ind, 2));
							skelClone.flip(ind);
						}
					}
				}
				
				//Traverse the skeleton until you reach the given endpoint
				//Keep track of the path length, and determine if it is
				//the longest of not
				while(true){
					current = queue.remove(0);
					startInd = current.i;
					length = current.length;
					if(startInd == endInd){
						if(length>longest){
							longest = length;
							endIndex[0] = i;
							endIndex[1] = j;
						}
						break;
					}
					x = startInd%width;
					y = startInd/width;
					for(int nx=x-1;nx<=x+1;nx++){
						if (nx<0 || nx>=width) continue;
						for(int ny=y-1;ny<=y+1;ny++){
							if(ny<0 || ny>=height) continue;
							ind = nx + ny*width;
							if(skelClone.get(ind)) {
								queue.add(new PathLength(ind, length+1));
								skelClone.flip(ind);
							}
						}
					}
				}
				
				
			}
		}
	}
	
	/**
	 * Method to make the VOI contour that will represent the polygonal
	 * area of the growth cone. Based on the branch tips VOI, which should
	 * have already been calculated, the points are used in the convex
	 * hull routine to generate the polygonal area curve. It tends to overestimate
	 * the area because it doesn't include all the tips.
	 * 
	 * The centroid of the polygonal area is calculated alongside the area.
	 */
	
	private void polygonalArea(){
		
		int xi, yi, ind;
		int numPts = tipPts.size();
		Vector3f[] points = new Vector3f[numPts];
		voiHull = new VOI((short) 2, "Polygonal Area", VOI.CONTOUR, -1);
		for(int i=0;i<tipPts.size();i++){
			//ind = tipPts.get(order[i]);
			ind = tipPts.get(i);
			xi = ind%width;
			yi = ind/width;
			points[i] = new Vector3f(xi,yi,0);
		}
		VOIContour curve = new VOIContour(true);
		curve.importPoints(points);
		curve.convexHull();
		voiHull.importCurve(curve);
		
		BitSet polyMask = voiHull.createBinaryMask(width, height, 0);
		int sumX = 0;
		int sumY = 0;
		int area = 0;
		for (int i = polyMask.nextSetBit(0); i >= 0; i = polyMask.nextSetBit(i+1)){
			xi = i%width;
			yi = i/width;
			sumX += xi;
			sumY += yi;
			area++;
		}
		centroidPts[0] = (float)sumX/(float)area;
		centroidPts[1] = (float)sumY/(float)area;
		polyArea = area;
		
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
		int bins = 1; //Histogram bins are for 4 pixels
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
	 * Gathers information and writes it to the SWC file
	 * 
	 * @param line which line in the SWC file we are on
	 * @param i index of the pixel we are at
	 * @param type whether it is a branch/fork or an endpoint. 
	 * 0 denotes a fork, -1 denotes the start point, and anything else
	 * is an endpoint
	 * @param connected the branch which this component is connected to
	 */
	
	private void writeInfo(int line, int i, int type, int connected){
		String typeStr;
		if(type == 0) typeStr = "5";
		else if(type == -1) typeStr = "2";
		else typeStr = "6";
		int x = i%width;
		int y = height - i/width;
		String output = String.valueOf(line) + " " + typeStr + " " 
				+ String.valueOf(x) + ".0 " + String.valueOf(y) + ".0 "
				+ "0.0 1.0 " + String.valueOf(connected) + " \n";
		try {
			swcOut.append(output);
			swcOut.flush();
		} catch (IOException e) {
			MipavUtil.displayError("Unable to write to SWC file");
			return;
		}
	}
	
	private class BranchContainer{
		
		private int i;
		
		private int line;
		
		private int originated;
		
		private BranchContainer(int index, int lineNum, int origin){
			i = index;
			line = lineNum;
			originated = origin;
		}
		
	}
	
	/**
	 * Structure to hold the current index of the path search
	 * as well as the current length of the path.  Used mostly 
	 * for storage purposes in the longest path method. 
	 * @author wangvg
	 *
	 */
	
	private class PathLength{
		
		private int i;
		
		private int length;
		
		private PathLength(int index, int number){
			i = index;
			length = number;
		}
	}
	
	/**
	 * A container class to hold all the previous states of the 
	 * skeleton and VOIs during modifications. This allows for
	 * rudimentary undo/redo functionality. 
	 * @author wangvg
	 *
	 */
	
	private class UndoContainer{
		
		private float[] lastCentroid;
				
		private VOI lastHull;
		
		private BitSet lastSkel;
		
		private ArrayList<Integer> lastTips;

		private UndoContainer(){
			
			lastTips = new ArrayList<Integer>();
		}
		
		/**
		 * Swaps the current state and previous state, which
		 * basically allows for one level of undo/redo
		 */
		
		private void recall(){
			
			if(lastSkel == null || lastTips == null
					|| lastHull == null || lastCentroid == null) return;
			
			ArrayList<Integer> tempList = new ArrayList<Integer>();
			tempList.addAll(tipPts);
			tipPts.clear();
			tipPts.addAll(lastTips);
			lastTips.clear();
			lastTips.addAll(tempList);
			
			
			BitSet tempSet = skeleton;
			skeleton = lastSkel;
			lastSkel = tempSet;
			
			VOI tempVOI = voiHull;
			voiHull = lastHull;
			lastHull = tempVOI;
			
			float[] tempFloat = centroidPts;
			centroidPts = lastCentroid;
			lastCentroid = tempFloat;

		}
		
		/**
		 * Stores the previous states in this class'
		 * variables
		 */
		
		private void update(){
			
			lastSkel = (BitSet) skeleton.clone();
			lastTips.clear();
			lastTips.addAll(tipPts);
			lastHull = (VOI) voiHull.clone();
			lastCentroid = centroidPts.clone();
		}	
	}
}
