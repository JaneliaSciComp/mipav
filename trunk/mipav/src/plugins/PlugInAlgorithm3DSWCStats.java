import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Map.Entry;
import java.util.TreeMap;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.view.MipavUtil;


public class PlugInAlgorithm3DSWCStats extends AlgorithmBase {

	private ArrayList<ArrayList<float[]>> swcCoordinates;
	
	//private File surfaceFile;
	
	private ArrayList<File> surfaceFiles;
	
	
	/*
	 * Was only for singular run, but now designed for bulk runs
	 * by choosing a directory
	 */
	
	/*public PlugInAlgorithm3DSWCStats(File surface){
		super();
		surfaceFile = surface;
		swcCoordinates = new ArrayList<ArrayList<float[]>>();
		
	}*/
	
	public PlugInAlgorithm3DSWCStats(ArrayList<File> surfaces){
		super();
		surfaceFiles = surfaces;
		swcCoordinates = new ArrayList<ArrayList<float[]>>();
	}
	
	@Override
	public void runAlgorithm() {
		
		for(File f : surfaceFiles){
			readSurfaceFile(f);
			calculateDistances();
			ArrayList<ArrayList<Integer>> forward = makeConnections();
			int maxOrder = determineOrder(forward);
			ArrayList<String> messages = consolidateFilaments(forward, maxOrder);
			recalculateDistances();
			addToMessages(messages);
			try {
				writeSWC(f, messages);
			} catch (IOException e) {
				MipavUtil.displayError("Cannot save to SWC file");
				return;
			}
		}

		setCompleted(true);

		
	}
	
	/**
	 * Determines the length of each individual filament
	 * read from the Imaris trace. These will be used
	 * to determine the axon filaments. 
	 */
	private void calculateDistances(){
		for(ArrayList<float[]> alf : swcCoordinates){
			float[] currPt = alf.get(0);
			for(int i=1;i<alf.size();i++){
				float[] nextPt = alf.get(i);
				float dist = 0;
				for(int j=0;j<3;j++){
					float diff = currPt[j] - nextPt[j];
					dist += diff*diff;
				}
				nextPt[3] = currPt[3] + (float)Math.sqrt(dist);
				currPt = nextPt;
			}
		}
	}
	
	/**
	 * Adds the branch length and distance along the
	 * axon/parent to the output messages. 
	 * @param messages
	 */
	private void addToMessages(ArrayList<String> messages){
		for(int i=0;i<messages.size();i++){
			String message = messages.get(i);
			ArrayList<float[]> fil = swcCoordinates.get(i);
			message += String.format("#Branch Length: %3.5f \n", 
					fil.get(fil.size()-1)[3]);
			if(i!=0){
				message += String.format("#Length along parent branch: %3.5f \n"
						, fil.get(0)[3]);
			}
			message += "#------------------------------------\n";
			messages.set(i, message);
		}
	}
	
	/**
	 * Recalculate distances for the consolidated branches
	 * so that it also includes the branch length and
	 * distance along the axon/parent this branch
	 * originates from. 
	 */
	private void recalculateDistances(){
		
		for(int i=0;i<swcCoordinates.size();i++){
			ArrayList<float[]> fil = swcCoordinates.get(i);
			float parentLength = 0;
			if(i==0){
				fil.get(0)[3] = 0;
			}else{
				float[] head = fil.get(0);
				int connection = (int) fil.get(0)[4]-1;
				ArrayList<float[]> list = null;
				for(int j=0;j<swcCoordinates.size();j++){
					list = swcCoordinates.get(j);
					if(connection >= list.size()){
						connection -= list.size();
					}else{
						break;
					}
				}
				float[] pt = list.get(connection);
				parentLength = pt[3];
				float dist = 0;
				for(int j=0;j<3;j++){
					float diff = pt[j] - head[j];
					dist += diff*diff;
				}
				head[3] = (float)Math.sqrt(dist);
			}
			float[] currPt = fil.get(0);
			for(int j=1;j<fil.size();j++){
				float[] nextPt = fil.get(j);
				float dist = 0;
				for(int k=0;k<3;k++){
					float diff = currPt[k] - nextPt[k];
					dist += diff*diff;
				}
				nextPt[3] = currPt[3] + (float)Math.sqrt(dist);
			}
			fil.get(0)[3] = parentLength;//head hold length along parent branch
		}
		//Should now have length for all branches
	}
	
	/**
	 * Build both forward and backwards connections based on
	 * the coordinates read from the Imaris trace. The 
	 * backwards connection routine is taken from the 
	 * Drosophila Registration algorithm written by
	 * Nish Pandya. 
	 * @return
	 */
	private ArrayList<ArrayList<Integer>> makeConnections(){
		
		//Forward connections
		ArrayList<ArrayList<Integer>> connections = new ArrayList<ArrayList<Integer>>();
		for(int i=0;i<swcCoordinates.size();i++){
			ArrayList<Integer> a = new ArrayList<Integer>();
			connections.add(a);
		}
		swcCoordinates.get(0).get(0)[4] = -1;
		for(int i=1;i<swcCoordinates.size();i++){
			ArrayList<float[]> fil = swcCoordinates.get(i);
			float[] head = fil.get(0);
			for(int j=0;j<i;j++){
				fil = swcCoordinates.get(j);
				float[] tail = fil.get(fil.size()-1);
				if(head[0] == tail[0] && head[1] == tail[1] && head[2] == tail[2]){
					head[4] = j;
					connections.get(j).add(i);
					break;
				}
			}
		}
		
		return connections;
	}
	
	private String formatSWCLine(int lineNum, float[] line) {
		String format = "%d %d %4.5f %4.5f %4.5f %4.2f %d \n";
		int type;
		if(line[5] == 1.0F){
			type = 2;
		}else{
			type = 3;
		}
		
		return String.format(format, lineNum, type, line[0], line[1], line[2], 0.1F, (int)line[4]);
	}
	
	private void writeSWC(File file, ArrayList<String> messages) throws IOException{
		String parent = file.getParent();
		String name = file.getName();
		name = name.substring(0, name.lastIndexOf("."));
		String output = parent + File.separator + name + ".swc";
		File outputFile = new File(output);
		
		FileWriter fw = new FileWriter(outputFile);

		int counter = 1;
		
		for(int i=0;i<swcCoordinates.size();i++){
			ArrayList<float[]> fil = swcCoordinates.get(i);
			String message = messages.get(i);
			fw.append(message);
			
			for(int j=0 ;j<fil.size();j++, counter++){
				fw.append(formatSWCLine(counter, fil.get(j)));
			}
		}
		
		fw.close();
	}
	
	/**
	 * Consolidate the multitude of filaments from the input to make
	 * stat tracking easier. Also organizes the output SWC so that
	 * branch numbers are logical and go from beggining of the axon
	 * to the end of the axon.
	 * @param connections
	 * @param maxOrder
	 * @return
	 */
	private ArrayList<String> consolidateFilaments(ArrayList<ArrayList<Integer>> connections, int maxOrder){
		
		ArrayList<ArrayList<float[]>> newFilaments = new ArrayList<ArrayList<float[]>>();
		
		ArrayList<float[]> current = new ArrayList<float[]>();
		ArrayList<String> messages = new ArrayList<String>();
		//Keep track of which branches we need to add.
		//We need deque to make sure branches are added in the 
		//correct and logical order.
		ArrayList<ArrayDeque<Integer>> dequeList = new ArrayList<ArrayDeque<Integer>>();
		for(int i=0;i<maxOrder;i++){
			dequeList.add(new ArrayDeque<Integer>());
		}
		
		//To keep track of branch numbering. 
		int[] branchNumber = new int[maxOrder];
		
		//Keep track of which line in the SWC this coordinate
		//is connected to.
		int counter = 1;
		int currentOrder = 0;//Will do zero indexing here
		ArrayList<float[]> fil = swcCoordinates.get(0);
		fil.get(0)[4] = -1; //set first line's connection to -1 (origin for SWC)
		fil.get(1)[4] = 1;
		fil.get(1)[5] = 1;
		current.add(fil.get(0));
		dequeList.get(0).add(0);
		
		boolean isFinished = false;
		
		while(!isFinished){
			//This looks confusing, but maybe the comments will be helpful
			ArrayDeque<Integer> currentFil = dequeList.get(currentOrder);
			int ind = currentFil.poll();
			ArrayList<Integer> connected = connections.get(ind);
			fil = swcCoordinates.get(ind);
			float thisOrder = fil.get(0)[5];
			float[] second = fil.get(1);
			second[5] = thisOrder;
			current.add(second);
			counter++;
			for(int i=2;i<fil.size();i++){
				float[] fa = fil.get(i);
				fa[4] = counter;
				fa[5] = thisOrder;
				current.add(fa);
				counter++;
			}
			
			//Reached the end of a branch, need to see if there
			//are child branches that need to be added as well
			if(connected.size() == 0){
				//Add the consolidated branch
				newFilaments.add(current);
				/*
				 * Concurrently prepare information regarding this branch.
				 * This includes branch order, number, length, and distance
				 * along the axon/parent branch that the branch originates
				 * from. The latter two are added later after recalculating
				 * distances.
				 */
				String message = "";
				if(currentOrder == 0){
					message = "#------------------------------------\n"
							+ "#Axon\n";
				}else{
					message = "#------------------------------------\n"
							+ "#Branch " + String.valueOf(branchNumber[1] + 1);
					for(int i=2;i<=currentOrder;i++){
						message += "." + String.valueOf(branchNumber[i] + 1);
					}
					message += "\n";
					message += "#Branch Order: " + String.valueOf(currentOrder);
					message += "\n";
				}
				
				messages.add(message);
				current = new ArrayList<float[]>();
				//Highest order branch, can't keep going higher
				if(currentOrder == maxOrder - 1){
					//Find the highest order which still needs
					//to consolidate branches
					if(dequeList.get(currentOrder).isEmpty()){
						while(dequeList.get(currentOrder).isEmpty()){
							branchNumber[currentOrder] = 0;
							currentOrder--;
							if(currentOrder == 0){
								isFinished = true;
								break;
							}
						}
						branchNumber[currentOrder]++;
					}
				}
				//Not highest order, check to see if you added any
				//child branches
				else if(dequeList.get(currentOrder+1).isEmpty()){
					while(dequeList.get(currentOrder).isEmpty()){
						branchNumber[currentOrder] = 0;
						currentOrder--;
						if(currentOrder == 0){
							isFinished = true;
							break;
						}
					}
					branchNumber[currentOrder]++;
				}else{
					currentOrder++;//Go to higher branch since you populated it
				}
				
			}
			//This is not an endpoint
			else{
				for(int i=0;i<connected.size();i++){
					/*
					 * Add forward connections to the respective deques.
					 * First element goes into the same deque because it
					 * is the same order as this current branch. Add at
					 * the front so that you prioritize continuing the 
					 * same branch.
					 * 
					 * Child branches (non-first element) are added to the
					 * end of the deques so that earlier children are
					 * written first. 
					 */
					int next = connected.get(i);
					fil = swcCoordinates.get(next);
					fil.get(1)[4] = counter;
					int order = (int) (fil.get(0)[5] - 1);
					if(i==0){
						dequeList.get(order).addFirst(next);
					}else{
						dequeList.get(order).add(next);
					}
					
				}
			}
		}
		
		swcCoordinates = newFilaments;
		
		return messages;
	}
	
	/**
	 * Three pass process to determine branch ordering and which filaments
	 * are the axon. Determines by finding the longest path from the first
	 * filament. 
	 * @param connections
	 */
	private int determineOrder(ArrayList<ArrayList<Integer>> connections){
		
		ArrayDeque<Integer> queue = new ArrayDeque<Integer>();
		
		ArrayList<float[]> fil = swcCoordinates.get(0);
		float[] head = fil.get(0);
		float[] tail = fil.get(fil.size()-1);
		float dist = tail[3];
		
		int maxOrder = 1;
		
		ArrayList<Integer> branches = connections.get(0);
		for(int i : branches){
			queue.add(i);
			fil = swcCoordinates.get(i);
			fil.get(0)[3] = dist;
		}
		
		TreeMap<Float, Integer> tmap = new TreeMap<Float, Integer>(new Comparator<Float>(){
			@Override
			public int compare(Float o1, Float o2) {
				float f1 = o1.floatValue();
				float f2 = o2.floatValue();
				if(f1 > f2)
					return -1;
				else if(f1 < f2) 
					return 1;
				else
					return 0;
			}
		});
		
		/* 
		 * Pass 1
		 * Accumulate the length of each filament. At the head
		 * of the filament, store the cumulative length for 
		 * later use. Once a tip has been reached, put the 
		 * length to that tip in a tree map.
		 * 
		 * Pretty much a BFS as the ArrayDeque is used as a queue
		 */
		while(!queue.isEmpty()){
			int i = queue.poll();
			fil = swcCoordinates.get(i);
			head = fil.get(0);
			tail = fil.get(fil.size()-1);
			dist = tail[3] + head[3];
			head[3] = dist;
			branches = connections.get(i);
			if(branches.isEmpty()){
				tmap.put(dist, i);
				tail[3] = dist;
			}
			for(int j : branches){
				queue.add(j);
				fil = swcCoordinates.get(j);
				fil.get(0)[3] = dist;
			}
		}
		
		/*
		 * Pass 2
		 * Tree map is based on length to the tips. Working
		 * backwards from the furthest points to the closest
		 * points should increase efficiency. 
		 * 
		 * Trace back from the tips and replace the tail
		 * length values to the length at the tip. If you
		 * are tracing backwards and the connected filament
		 * already has been reached by a longer filament 
		 * (which should always be the case because of the
		 * tree map), then move on to the next tip.
		 * 
		 * Rearrange forward connections so that the first one
		 * in the list is the longest path away from this 
		 * filament.
		 */
		while(!tmap.isEmpty()){
			Entry<Float, Integer> entry = tmap.pollFirstEntry();
			int i = entry.getValue();
			dist = entry.getKey();
			fil = swcCoordinates.get(i);
			int c = (int)fil.get(0)[4];
			int prev = i;
			while(c > -1){
				fil = swcCoordinates.get(c);
				head = fil.get(0);
				tail = fil.get(fil.size()-1);
				if(tail[3] < dist){
					tail[3] = dist;
					branches = connections.get(c);
					branches.remove(new Integer(prev));
					branches.add(0, prev);
					prev = c;
					c = (int) head[4];
				}else{
					c = -1;
				}
			}
		}
		
		/*
		 * Pass 3
		 * Forward connections are organized so that the longest 
		 * path is the first element. Increment the branch order
		 * of all the other elements in the list. Traverse the
		 * entire neuron in a BFS. 
		 */
		
		fil = swcCoordinates.get(0);
		head = fil.get(0);
		head[5] = 1;
		
		branches = connections.get(0);
		for(int i=0;i<branches.size();i++){
			int ind = branches.get(i);
			queue.add(ind);
			fil = swcCoordinates.get(i);
		}
		
		for(int i=0;i<branches.size();i++){
			int ind = branches.get(i);
			fil = swcCoordinates.get(ind);
			if(i == 0){
				fil.get(0)[5] = head[5];
			}else{
				fil.get(0)[5] = head[5] + 1;
			}
		}
		
		while(!queue.isEmpty()){
			int i = queue.poll();
			fil = swcCoordinates.get(i);
			head = fil.get(0);
			branches = connections.get(i);
			if(branches.size() == 0)
				continue;
			for(int j=0;j<branches.size();j++){
				int ind = branches.get(j);
				queue.add(ind);
				fil = swcCoordinates.get(j);
			}
			
			for(int j=0;j<branches.size();j++){
				int ind = branches.get(j);
				fil = swcCoordinates.get(ind);
				if(j == 0){
					fil.get(0)[5] = head[5];
				}else{
					fil.get(0)[5] = head[5] + 1;
					if(head[5] + 1 > maxOrder){
						maxOrder = (int) (head[5] + 1);
					}
				}
			}
		}
		
		return maxOrder;
		
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
							 */
							float[] coords = {coord_x,coord_y,coord_z,0,0,0};
							
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
	
	/*private void testSWCOut(ArrayList<ArrayList<Integer>> connections) throws IOException{
	
		float[] origin = swcCoordinates.get(0).get(0);
		origin[4] = -1;
		ArrayList<float[]> fil = swcCoordinates.get(0);
		fil.get(0)[4] = -1;
		fil.get(1)[4] = 1;
		
		int counter = 1;
		
		for(int i=0;i<swcCoordinates.size();i++){
			fil = swcCoordinates.get(i);
			ArrayList<Integer> branches = connections.get(i);
			counter++;
			for(int j=2;j<fil.size();j++, counter++){
				fil.get(j)[4] = counter;
			}
			for(int j : branches){
				fil = swcCoordinates.get(j);
				fil.get(1)[4] = counter;
			}
		}
		
		String parent = surfaceFile.getParent();
		String name = surfaceFile.getName();
		name = name.substring(0, name.lastIndexOf("."));
		String output = parent + File.separator + name + ".swc";
		File outputFile = new File(output);
		
		FileWriter fw = new FileWriter(outputFile);
		
		counter = 1;
		
		fw.append(formatSWCLine(counter, swcCoordinates.get(0).get(0)));
		counter++;
		for(int i=0;i<swcCoordinates.size();i++){
			fil = swcCoordinates.get(i);
			for(int j=1;j<fil.size();j++, counter++){
				fw.append(formatSWCLine(counter, fil.get(j)));
			}
		}
		
		fw.close();
	
	}*/

}
