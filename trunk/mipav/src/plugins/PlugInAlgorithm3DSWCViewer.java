import java.awt.Color;
import java.awt.Point;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.BitSet;

import javax.swing.JTextPane;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.ViewJFrameImage;

/**
 * A sister plugin to the 3DSWCStats set of plugins. The dialog opens up
 * a very rudimentary 3D viewer of the neuron skeleton. The user can
 * then choose which branch to use as the axon when exported to a SWC
 * and in the stats CSV. This algorithm is very similar to the original
 * 3DSWCStats since all the methods still occur. However, the dialog
 * allows the user to choose an axon and thus requires the connections
 * in this algorithm to be rearranged.
 * @see PlugInAlgorithm3DSWCStats 
 * @author wangvg
 *
 */
public class PlugInAlgorithm3DSWCViewer extends AlgorithmBase {

	public static int SETUP = 0;
	
	public static int WRITE = 1;
	
	private int algStep = SETUP;
	
	private ArrayList<ArrayList<float[]>> swcCoordinates;
	
	private ArrayList<ArrayList<Integer>> connections;
	
	private ArrayList<float[]> joints;
	
	private JTextPane textArea;
	
	private boolean disconnected;
	
	private File swcFile;
	
	private int currentAxon;
	
	private String resolutionUnit;
	
	private SimpleAttributeSet attr;
	
	private ArrayList<float[]> spacePts;
	
	private TransMatrix mat; 
	
	public PlugInAlgorithm3DSWCViewer(File file, JTextPane text, String resUnit){
		
		super();
		
		destImage = new ModelImage(ModelImage.BOOLEAN, new int[]{512, 512}, "");
		
		String name = file.getName();
		name = name.substring(0, name.lastIndexOf("."));
		destImage.setImageName(name);
		
		swcFile = file;
		textArea = text;
		resolutionUnit = resUnit;
	}
	
	@Override
	/**
	 * In this algorithm, only the setup steps are carried out. 
	 * These steps are reading the Imaris file, attempting to
	 * create connections, and building the basic structure 
	 * for output to the 3D viewer. The writing steps are handled
	 * in a different method. 
	 */
	public void runAlgorithm() {
		
		attr = new SimpleAttributeSet();
		StyleConstants.setFontFamily(attr, "Serif");
		StyleConstants.setFontSize(attr, 12);

		SimpleAttributeSet redText = new SimpleAttributeSet(attr);
		StyleConstants.setForeground(redText, Color.red.darker());
		
		try{
		
			swcCoordinates = new ArrayList<ArrayList<float[]>>();
			joints = new ArrayList<float[]>();
			mat = new TransMatrix(3);
	
			append("Reading " + swcFile.getName(), attr);
			readSurfaceFile(swcFile);
	
			disconnected = false;
	
			connections = makeConnections();
	
			for(int i=1;i<swcCoordinates.size();i++){
				ArrayList<float[]> fil = swcCoordinates.get(i);
				if(fil.get(0)[4] == Float.NEGATIVE_INFINITY){
					//No connection was made, something is wrong
					disconnected = true;
					break;
				}
			}
			if(disconnected){
				//Try version with tolerance
				connections = makeConnectionsTol();
				//Test out one more time
				for(int i=1;i<swcCoordinates.size();i++){
					ArrayList<float[]> fil = swcCoordinates.get(i);
					if(fil.get(0)[4] == Float.NEGATIVE_INFINITY){
						//No connection was made, something is wrong
						append(swcFile.getName() + " is not connected properly.", redText);
						//allGood = false;
						setCompleted(false);
						return;
					}
				}
			}
	
			joints.add(swcCoordinates.get(0).get(0));
			for(int i=0;i<swcCoordinates.size();i++){
				ArrayList<float[]> fil = swcCoordinates.get(i);
				joints.add(fil.get(fil.size()-1));
			}
	
			//Make the viewer image here
			setupImage();
		}catch(Exception e){
			append("The following Java error has occured:", redText);
			append(e.toString(), redText);
			for(StackTraceElement t : e.getStackTrace())
				append(t.toString(), redText);
			setCompleted(false);
			return;
		}
		setCompleted(true);
		
	}
	
	/**
	 * This method handles the actual writing and calculation steps
	 * after the user has chosen an axon. Other than rearranging
	 * the order of forward connections, this portion is more or
	 * less the same as the back half of the sibling algorithm.
	 */
	public void write(){
		
		SimpleAttributeSet redText = new SimpleAttributeSet(attr);
		StyleConstants.setForeground(redText, Color.red.darker());
		
		boolean allGood = true;
		
		try{
			rearrangeBranches();
			calculateDistances();
			int maxOrder = determineOrder(connections);
			ArrayList<String> messages = consolidateFilaments(connections, maxOrder);
			float[] branchLengths = recalculateDistances();
			addToMessages(messages);
			try {
				String output = writeSWC(swcFile, messages, branchLengths);
				append("Converted to SWC -> " + output, attr);
			} catch (IOException e) {
				append("Could not write SWC for " + swcFile.getName(), redText);
				allGood = false;
			}
			try{
				String output = exportStatsToCSV(swcFile, messages, branchLengths);
				append("Exported stats to CSV -> " + output, attr);
			} catch (IOException e) {
				append("Could not export stats to CSV for " + swcFile.getName(), redText);
				allGood = false;
			}
		}catch(Exception e){
			append("The following Java error has occured:", redText);
			append(e.toString(), redText);
			for(StackTraceElement t : e.getStackTrace())
				append(t.toString(), redText);
			allGood = false;
		}
		
		setCompleted(allGood);
	}
	
	public int getAlgStep(){
		return algStep;
	}
	
	public void setAlgStep(int step){
		algStep = step;
	}
	
	/**
	 * Rotates the image and makes the projection to
	 * be displayed in the image frame. Also includes
	 * zoom factor now.
	 * @param rx
	 * @param ry
	 * @param rz
	 * @param zoom
	 */
	public void transformImage(int tx, int ty, int rx, int ry, int rz, double zoom){
		
		mat = new TransMatrix(3);
		//if you want to zoom, do it AFTER rotate
		mat.setRotate(rx, ry, rz, TransMatrix.DEGREES);
		mat.setZoom(zoom, zoom);
		
		
		//Rotate about the center of the image
		for(int i=0;i<joints.size();i++){
			float[] joint = joints.get(i);
			float[] transJoint = new float[3];
			for(int j=0;j<2;j++){
				transJoint[j] = joint[j] - 256;
			}
			transJoint[2] = joint[2];
			float[] rotJoint = new float[3];
			mat.transform(transJoint, rotJoint);
			for(int j=0;j<2;j++){
				rotJoint[j] += 256;
			}
			rotJoint[0] += tx;
			rotJoint[1] += ty;
			
			spacePts.set(i, rotJoint);
			
		}
		
		makeViewImage();
		
		BitSet axonMask = highlightAxon(currentAxon);
		ViewJFrameImage frame = destImage.getParentFrame();
		
		frame.getComponentImage().setPaintMask(axonMask);
		frame.getControls().getTools().setOpacity(1.0f);
		frame.getControls().getTools().setPaintColor(Color.RED);
		
		frame.updateImages(true);
	}
	
	public TransMatrix getMatrix(){
		return mat;
	}
	
	public TransMatrix mouseRotate(int tx, int ty, int rx, int ry){
		//Want to rotate about center of neuron, so recenter the points
		
		mat.setRotate(rx, ry, 0, TransMatrix.DEGREES);
		
		for(int i=0;i<joints.size();i++){
			float[] pt = joints.get(i);
			float[] tPt = new float[3];
			tPt[0] = pt[0] - 256;
			tPt[1] = pt[1] - 256;
			tPt[2] = pt[2];
			float[] rPt = new float[3];
			mat.transform(tPt, rPt);
			rPt[0] += 256 + tx;
			rPt[1] += 256 + ty;
			
			spacePts.set(i, rPt);
		}
		
		makeViewImage();
		
		BitSet axonMask = highlightAxon(currentAxon);
		ViewJFrameImage frame = destImage.getParentFrame();
		
		frame.getComponentImage().setPaintMask(axonMask);
		frame.getControls().getTools().setOpacity(1.0f);
		frame.getControls().getTools().setPaintColor(Color.RED);
		
		frame.updateImages(true);
		
		return mat;
		
	}
	
	public void mouseTranslate(int tx, int ty){
		for(int i=0;i<spacePts.size();i++){
			float[] pt = spacePts.get(i);
			pt[0] += tx;
			pt[1] += ty;
		}
		
		makeViewImage();
		
		BitSet axonMask = highlightAxon(currentAxon);
		ViewJFrameImage frame = destImage.getParentFrame();
		
		frame.getComponentImage().setPaintMask(axonMask);
		frame.getControls().getTools().setOpacity(1.0f);
		frame.getControls().getTools().setPaintColor(Color.RED);
		
		frame.updateImages(true);
	}

	/**
	 * Using the selected branch, create a bitset to be used
	 * as the image mask that overlays both the branch that
	 * was selected as well as the connections all the way
	 * back to the origin. 
	 * @param branch
	 * @return
	 */
	public BitSet highlightAxon(int branch){
		
		currentAxon = branch;
		
		BitSet axonMask = new BitSet(512*512);
		
		ArrayList<float[]> fil = swcCoordinates.get(branch);
		int c = (int) fil.get(0)[4];
		
		ArrayList<ArrayList<Point>> lines = new ArrayList<ArrayList<Point>>();
		
		float[] oPt = spacePts.get(0);
		Point origin = new Point(Math.round(oPt[0]), Math.round(oPt[1]));
		float[] nPt = spacePts.get(1);
		Point pt = new Point(Math.round(nPt[0]), Math.round(nPt[1]));
		
		lines.add(bresenham(origin, pt));
		
		while(c > -1){
			oPt = spacePts.get(branch+1);
			nPt = spacePts.get(c+1);
			origin = new Point(Math.round(oPt[0]), Math.round(oPt[1]));
			pt = new Point(Math.round(nPt[0]), Math.round(nPt[1]));
			lines.add(bresenham(origin, pt));
			
			fil = swcCoordinates.get(c);
			branch = c;
			c = (int) fil.get(0)[4];
		}
		
		for(int i=0;i<lines.size();i++){
			ArrayList<Point> line = lines.get(i);
			for(int j=0;j<line.size();j++){
				pt = line.get(j);
				if(pt.x > 0 && pt.x < 512 &&
						pt.y > 0 && pt.y < 512)
					axonMask.set(pt.x + pt.y*512);
			}
		}
		
		return axonMask;
	}

	/**
	 * Gets all branches that have no forward connections
	 * (and thus would be a tip). This will be used to
	 * populate the list of potential axon branches. 
	 * @return
	 */
	public ArrayList<Integer> getTips(){
		ArrayList<Integer> tips = new ArrayList<Integer>();
		for(int i=0;i<connections.size();i++){
			ArrayList<Integer> branches = connections.get(i);
			if(branches.size()==0){
				tips.add(i);
			}
		}
		
		return tips;
	}

	public int getAxon(){
		return currentAxon;
	}

	/**
	 * Determines the length of each individual filament
	 * read from the Imaris trace. These will be used
	 * to determine the axon filaments. 
	 */
	private void calculateDistances(){
		if(disconnected){//Distance at head of filament is not 0
			for(int i=0;i<swcCoordinates.size();i++){
				ArrayList<float[]> fil = swcCoordinates.get(i);
				if(i==0){
					fil.get(0)[3] = 0;
				}else{
					float[] head = fil.get(0);
					int c = (int) fil.get(0)[4];
					ArrayList<float[]> conn = swcCoordinates.get(c);
					float[] tail = conn.get(conn.size()-1);
					
					
					float dist = 0;
					for(int j=0;j<3;j++){
						float diff = tail[j] - head[j];
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
					currPt = nextPt;
				}
			}
		}else{//Distance at head of filament is 0
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
			message += String.format("# Branch Length: %3.5f %s\n", 
					fil.get(fil.size()-1)[3], resolutionUnit);
			String parent;
			if(i!=0){
				if(i==1)
					parent = "axon";
				else parent = "parent branch";
				message += String.format("# Length along %s: %3.5f %s\n"
						, parent, fil.get(0)[3], resolutionUnit);
			}
			message += "#------------------------------------\n";
			messages.set(i, message);
		}
	}
	
	private String exportStatsToCSV(File file, ArrayList<String> messages, float[] branchLengths) throws IOException{
		String parent = file.getParent();
		String name = file.getName();
		name = name.substring(0, name.lastIndexOf("."));
		String output = parent + File.separator + name + "_stats.csv";
		File outputFile = new File(output);
		
		FileWriter fw = new FileWriter(outputFile);
		
		fw.append("Units," + resolutionUnit + "\n");
		
		String branchInfo = "";
		branchInfo += "Total branch length," + String.valueOf(branchLengths[0]) + "\n";
		branchInfo += "Minus axon," + String.valueOf(branchLengths[1]) + "\n\n";
		
		fw.append(branchInfo);
		
		String header = "Branch Number, Branch Order, Branch Length, Length along parent \n";
		
		fw.append(header);
		
		for(String s : messages){
			StringBuilder sb = new StringBuilder(30);
			String[] rows = s.split("\n");
			int rowNum = 1;
			
			//Write branch number (or axon);
			String branch = rows[rowNum].replace("#", "").trim();
			String[] branchSplit = branch.split(" ");
			if(branchSplit.length == 1){
				sb.append("Axon");
				sb.append(",");
				sb.append("0"); //Write axon
				rowNum++;
			}else{
				sb.append(branchSplit[1]);
				sb.append(",");
				rowNum++;
				//Write branch order
				String order = rows[rowNum].replace("#", "").trim();
				String[] orderSplit = order.split(" ");
				sb.append(orderSplit[2]);
				rowNum++;
			}
			sb.append(",");
			
			//Write length
			String length = rows[rowNum].replace("#", "").trim();
			String[] lengthSplit = length.split(" ");
			sb.append(lengthSplit[2]);
			sb.append(",");
			rowNum++;
			
			//Write length along parent
			if(branchSplit.length == 1){
				sb.append("Axon");
			}else{
				String along = rows[rowNum].replace("#", "").trim();
				String[] alongSplit = along.split(" ");
				sb.append(alongSplit[alongSplit.length - 2]);
			}
			sb.append("\n");
			fw.append(sb.toString());
		}
		
		fw.close();
		
		return output;
	}
	
	/**
	 * Recalculate distances for the consolidated branches
	 * so that it also includes the branch length and
	 * distance along the axon/parent this branch
	 * originates from. 
	 */
	private float[] recalculateDistances(){
		//0 => Total branch length, 1=> Higher order branch length
		float[] branchLengths = new float[2];
		
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
				currPt = nextPt;
			}
			branchLengths[0] += currPt[3];
			fil.get(0)[3] = parentLength;//head hold length along parent branch
		}
		
		ArrayList<float[]> axon = swcCoordinates.get(0);
		float axonLength = axon.get(axon.size()-1)[3];
		branchLengths[1] = branchLengths[0] - axonLength;
		
		//Should now have length for all branches
		
		return branchLengths;
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
		
		int offset;
		if(disconnected){
			offset = 0;
		}else{
			offset = 1;
		}
		
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
			float[] second = fil.get(offset);
			second[5] = thisOrder;
			current.add(second);
			counter++;
			for(int i=offset+1;i<fil.size();i++){
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
							+ "# Axon\n";
				}else{
					message = "#------------------------------------\n"
							+ "# Branch " + String.valueOf(branchNumber[1] + 1);
					for(int i=2;i<=currentOrder;i++){
						message += "." + String.valueOf(branchNumber[i] + 1);
					}
					message += "\n";
					message += "# Branch Order: " + String.valueOf(currentOrder);
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
					fil.get(offset)[4] = counter;
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
	 * Imaris filament files are patterned in a way that branch organization 
	 * can be inferred from it. Simplified the algorithm to determine the 
	 * axon/branch number so that it is based on the pattern seen in the files.
	 * 
	 * Basically take the last step of the three pass method earlier since
	 * in forward connections, the lower number is the one that goes
	 * towards the axon and others are child branches. 
	*/
	private int determineOrder(ArrayList<ArrayList<Integer>> connections){
		
		int maxOrder = 1;
		
		ArrayDeque<Integer> queue = new ArrayDeque<Integer>();
		
		ArrayList<float[]> fil = swcCoordinates.get(0);
		ArrayList<Integer> branches = connections.get(0);
		float[] head = fil.get(0);
		
		head[5] = 1;
		
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
	
	private String writeSWC(File file, ArrayList<String> messages, float[] branchLengths) throws IOException{
		String parent = file.getParent();
		String name = file.getName();
		name = name.substring(0, name.lastIndexOf("."));
		String output = parent + File.separator + name + ".swc";
		File outputFile = new File(output);
		
		FileWriter fw = new FileWriter(outputFile);
		
		String header = 
				  "#-----------------------------------------------------------------\n"
				+ "# SWC generated in MIPAV\n"
				+ "#-----------------------------------------------------------------\n"
				+ "# Organization of branches is as such:\n"
				+ "# -Axon is the first filament written (and noted as such)\n"
				+ "# -Branches are written in order of closest to its parent's\n"
				+ "#  origin\n"
				+ "# -Higher order branches are given further identification\n"
				+ "# \n"
				+ "# For example: \n"
				+ "# Branch 1 is the closest child branch of where the axon\n"
				+ "# originates and Branch 2 is the second closest child branch.\n"
				+ "# Branch 1.1 is the closest child branch from where the\n"
				+ "# first branch originated from.\n"
				+ "#-----------------------------------------------------------------\n"
				+ "# Branch Length Information\n"
				+ "# Total branch length: " + String.valueOf(branchLengths[0]) + " " + resolutionUnit + "\n"
				+ "# Minus axon: " + String.valueOf(branchLengths[1]) + " " + resolutionUnit + "\n"
				+ "#-----------------------------------------------------------------\n"
				+ "# Begin SWC Coordinates\n";

		fw.append(header);
		
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
		
		return output;
	}
	
	/**
	 * Using the selected branch, make sure that it comes first
	 * in the forward connections. 
	 */
	private void rearrangeBranches(){
		
		ArrayList<float[]> fil = swcCoordinates.get(currentAxon);
		int change = currentAxon;
		int c = (int)fil.get(0)[4];
		
		while(c > -1){
			ArrayList<Integer> branches = connections.get(c);
			branches.remove(new Integer(change));
			branches.add(0, change);
			fil = swcCoordinates.get(c);
			change = c;
			c = (int)fil.get(0)[4];
		}
	}
	
	/**
	 * The setup for making the 3D viewer. Translates the 
	 * branch into the center of a 512x512 image with a
	 * at least a 20 pixel pad on each dimension in the
	 * base (no rotation) image. 
	 */
	private void setupImage(){
		float[] minBounds = new float[]{Float.MAX_VALUE, Float.MAX_VALUE, Float.MAX_VALUE};
		float[] maxBounds = new float[]{-Float.MAX_VALUE, -Float.MAX_VALUE, -Float.MAX_VALUE};
		
		for(int i=0;i<joints.size();i++){
			float[] joint = joints.get(i);
			for(int j=0;j<3;j++){
				if(joint[j] < minBounds[j])
					minBounds[j] = joint[j];
				if(joint[j] > maxBounds[j])
					maxBounds[j] = joint[j];
			}
		}
		
		float xDiff = maxBounds[0] - minBounds[0];
		float yDiff = maxBounds[1] - minBounds[1];
		float zSum = maxBounds[2] + minBounds[2];
		
		float scale = 471.0F/Math.max(xDiff, yDiff);
		
		float xPad = (512.0F - xDiff*scale)/2.0F;
		float yPad = (512.0F - yDiff*scale)/2.0F;
		float zCenter = zSum / 2.0F;
		
		spacePts = new ArrayList<float[]>();

		for(int i=0;i<joints.size();i++){
			float[] joint = joints.get(i);
			float x = joint[0];
			float y = joint[1];
			float z = joint[2];
			
			x = (x - minBounds[0])*scale + xPad; 
			y = (y - minBounds[1])*scale + yPad;
			z = (z - zCenter)*scale;
			
			float[] fPt = new float[]{x, y, z};
			joints.set(i, fPt);
			spacePts.add(fPt);
		}
		
		makeViewImage();
		
		currentAxon = getTips().get(0);
		
	}
	
	/**
	 * Draws all the branches on the image based on
	 * the transform variables made in the earlier
	 * portions. 
	 */
	private void makeViewImage(){
		
		
		BitSet skeleton = new BitSet(512*512);
		
		ArrayList<ArrayList<Point>> lines = new ArrayList<ArrayList<Point>>();
		
		float[] oPt = spacePts.get(0);
		Point origin = new Point(Math.round(oPt[0]), Math.round(oPt[1]));
		float[] nPt = spacePts.get(1);
		Point pt = new Point(Math.round(nPt[0]), Math.round(nPt[1]));
		
		lines.add(bresenham(origin, pt));
		
		for(int i=0;i<connections.size();i++){
			oPt = spacePts.get(i+1);
			origin = new Point(Math.round(oPt[0]), Math.round(oPt[1]));
			ArrayList<Integer> branches = connections.get(i);
			for(int j : branches){
				nPt = spacePts.get(j+1);
				pt = new Point(Math.round(nPt[0]), Math.round(nPt[1]));
				lines.add(bresenham(origin, pt));
			}
		}
		for(int i=0;i<lines.size();i++){
			ArrayList<Point> line = lines.get(i);
			for(int j=0;j<line.size();j++){
				pt = line.get(j);
				if(pt.x > 0 && pt.x < 512 &&
						pt.y > 0 && pt.y < 512)
					skeleton.set(pt.x + pt.y*512);
			}
		}
		
		try {
			destImage.importData(0, skeleton, true);
		} catch (IOException e) {
			e.printStackTrace();
		}
		
	}
	
	/**
	 * Bresenham line algorithm used to draw the paths between two points.
	 * This version is used with the subclasses to keep track of which
	 * lines are displayed in the active slice to make it easier to add
	 * nodes between points
	 * @param p0
	 * @param p1
	 * @return
	 */
	private ArrayList<Point> bresenham(Point p0, Point p1){
		
		int x0 = p0.x;
		int x1 = p1.x;
		int y0 = p0.y;
		int y1 = p1.y;
		
		ArrayList<Point> pts = new ArrayList<Point>();
		int dx = Math.abs(x1-x0);
		int dy = Math.abs(y1-y0);
		int sx, sy;
		int err, e2;
		if(x0 < x1)
			sx = 1;
		else sx = -1;
		if(y0 < y1)
			sy = 1;
		else sy = -1;
		err = dx - dy;
		
		while(true){
			pts.add(new Point(x0, y0));

			if(x0 == x1 && y0 == y1) break;
			e2 = 2*err;
			if(e2 > -dy){
				err -= dy;
				x0 += sx;
			}
			if(e2 < dx){
				err += dx;
				y0 += sy;
			}
		}
		
		return pts;
	}
	
	
	private void append(String message, AttributeSet a){
		Document doc = textArea.getDocument();
		try {
			doc.insertString(doc.getLength(), message + "\n", a);
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
		
		textArea.setCaretPosition(doc.getLength());
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
			for(int j=i-1;j>=0;j--){
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
	
	/**
	 * Build both forward and backwards connections based on
	 * the coordinates read from the Imaris trace. The 
	 * backwards connection routine is taken from the 
	 * Drosophila Registration algorithm written by
	 * Nish Pandya. This version includes a tolerance because
	 * the traces Akanni gave me do not overlap and thus need
	 * to be connected a little more loosely.
	 * @return
	 */
	private ArrayList<ArrayList<Integer>> makeConnectionsTol(){
		
		float tolerance = 0.15F;
		
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
			for(int j=i-1;j>=0;j--){
				fil = swcCoordinates.get(j);
				float[] tail = fil.get(fil.size()-1);
				float dist = 0;
				for(int k=0;k<3;k++){
					float diff = head[k] - tail[k];
					dist += diff*diff;
				}
				
				if(dist < tolerance){//To deal with non-overlapping filaments
					head[4] = j;
					connections.get(j).add(i);
					break;
				}
			}
		}
		
		return connections;
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
							float[] coords = {coord_x,coord_y,coord_z,0,Float.NEGATIVE_INFINITY,0};
							
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

}
