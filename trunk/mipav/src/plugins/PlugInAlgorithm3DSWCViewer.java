import java.awt.Color;
import java.awt.Point;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.Comparator;
import java.util.Hashtable;
import java.util.Set;
import java.util.TreeMap;
import java.util.Map.Entry;

import javax.swing.JOptionPane;
import javax.swing.JTextPane;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

import de.jtem.numericalMethods.util.Arrays;
import quickhull3d.Point3d;
import quickhull3d.QuickHull3D;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmMaximumIntensityProjection;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIPoint;
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
public class PlugInAlgorithm3DSWCViewer extends AlgorithmBase{
	
	private ArrayList<ArrayList<Integer>> connections;
	
	private int currentAxon;
	
	private boolean disconnected;
	
	private ArrayList<float[]> joints;
	
	private TransMatrix mat;
	
	private String resolutionUnit;
	
	private ArrayList<float[]> spacePts;
	
	private ArrayList<ArrayList<float[]>> swcCoordinates;
	
	private File swcFile;
	
	private JTextPane textArea;
	
	private boolean axonUseLength;
	
	private boolean showAxon;
	
	private String imageFile;
	
	private boolean showViewer;
	
	private boolean viewerOpen;
	
	//private ModelImage splitImage;
	
	private final SimpleAttributeSet blackText;
	
	private final SimpleAttributeSet redText;
	
	private boolean branchDensity;
	
	private int[][] faceVerticies;
	
	//private Point3d[] verticies;
	
	private ArrayList<Integer> tips;
	
	private int[] vertexInd;
	
	private float splitDist;
	
	public PlugInAlgorithm3DSWCViewer(String imFile, File file, JTextPane text, String resUnit, boolean useLength, boolean showView){
		
		super();
		
		imageFile = imFile;
		//destImage = new ModelImage(ModelImage.BOOLEAN, new int[]{512, 512}, "3D Neuron Viewer");
		destImage = new ModelImage(ModelImage.ARGB, new int[]{512, 512}, "3D Neuron Viewer");
		
		String name = file.getName();
		name = name.substring(0, name.lastIndexOf("."));
		destImage.setImageName(name);
		
		swcFile = file;
		textArea = text;
		resolutionUnit = resUnit;
		axonUseLength = useLength;
		showAxon = true;
		showViewer = showView;
		branchDensity = false;
		
		viewerOpen = false;
		
		blackText = new SimpleAttributeSet();
		StyleConstants.setFontFamily(blackText, "Serif");
		StyleConstants.setFontSize(blackText, 12);

		redText = new SimpleAttributeSet(blackText);
		StyleConstants.setForeground(redText, Color.red.darker());
	}
	
	/**
	 * For branch density
	 * @param file
	 * @param text
	 * @param resUnit
	 */
	public PlugInAlgorithm3DSWCViewer(File file, JTextPane text, String resUnit){
		super();
		
		//destImage = new ModelImage(ModelImage.BOOLEAN, new int[]{512, 512}, "3D Neuron Viewer");
		destImage = new ModelImage(ModelImage.ARGB, new int[]{512, 512}, "3D Neuron Viewer");
		
		swcFile = file;
		textArea = text;
		resolutionUnit = resUnit;
		axonUseLength = false;
		showAxon = true;
		showViewer = true;
		branchDensity = true;
		
		viewerOpen = false;
		
		blackText = new SimpleAttributeSet();
		StyleConstants.setFontFamily(blackText, "Serif");
		StyleConstants.setFontSize(blackText, 12);

		redText = new SimpleAttributeSet(blackText);
		StyleConstants.setForeground(redText, Color.red.darker());
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
		
		try{
		
			swcCoordinates = new ArrayList<ArrayList<float[]>>();
			joints = new ArrayList<float[]>();
			mat = new TransMatrix(3);
	
			StyleConstants.setBold(blackText, true);
			
			append("Reading " + swcFile.getName(), blackText);
			
			StyleConstants.setBold(blackText, false);
			
			readSurfaceFile(swcFile);
	
			disconnected = false;
	
			connections = makeConnections(swcCoordinates);
	
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
				connections = makeConnectionsTol(swcCoordinates);
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
			
			tips = new ArrayList<Integer>();
			for(int i=0;i<connections.size();i++){
				ArrayList<Integer> branches = connections.get(i);
				if(branches.size()==0){
					tips.add(i);
				}
			}
	
			if(showViewer){
				joints.add(swcCoordinates.get(0).get(0));
				for(int i=0;i<swcCoordinates.size();i++){
					ArrayList<float[]> fil = swcCoordinates.get(i);
					joints.add(fil.get(fil.size()-1));
				}
		
				//Make the viewer image here
				setupImage();
				viewerOpen = true;
			}
			
			int tipSize = tips.size();
			
			int[][] temp = new int[2*tipSize][];
			vertexInd = new int[tipSize];
			
			int actualNum = calculateConvexHull(swcCoordinates, tips, temp, vertexInd);
			faceVerticies = new int[actualNum][];
			for(int i=0;i<actualNum;i++){
				faceVerticies[i] = temp[i];
			}
			Arrays.resize(vertexInd, actualNum);
			
			if(!branchDensity){
				
				append("Opening image " + imageFile, blackText);
				FileIO reader = new FileIO();
				srcImage = reader.readImage(imageFile);
				
				if(!showViewer){
					calculateDistances(swcCoordinates);
					if(axonUseLength){
						determineOrder_useLength(swcCoordinates, connections, currentAxon);
					}else{
						determineOrder(swcCoordinates, connections, currentAxon);
					}
				}
				
				//setupSplitChooser();
				
				//StyleConstants.setBold(blackText, true);
				
				//append("Select split point from image", blackText);
				
				//StyleConstants.setBold(blackText, false);
			}else{
				currentAxon = tips.get(0);
			}
			
			setCompleted(true);
			
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
	
	public void setSplit(float dist){
		splitDist = dist;
	}
	
	public void setAxon(int axon){
		currentAxon = axon;
	}
	
	public int getAxon(){
		return currentAxon;
	}

	/**
	 * Gets all branches that have no forward connections
	 * (and thus would be a tip). This will be used to
	 * populate the list of potential axon branches. 
	 * @return
	 */
	public ArrayList<Integer> getTips(){
		return tips;
	}
	
	public boolean isViewerOpen(){
		return viewerOpen;
	}
	
	public void viewerClosed(){
		viewerOpen = false;
		/*if(splitImage != null){
			splitImage.getParentFrame().close();
		}*/
	}

	/**
	 * Used as part of the action for dragging the 
	 * mouse with the left click. This applies a 
	 * rotation based on the mouse movement and
	 * returns the new rotation angles, which is
	 * then used in the transform method. 
	 * 
	 * @param rx
	 * @param ry
	 * @return
	 */
	public int[] mouseRotate(int rx, int ry){
		//Want to rotate about center of neuron, so recenter the points
		
		mat.setRotate(rx, ry, 0, TransMatrix.DEGREES);
		
		Vector3f[] bases = new Vector3f[3];
		bases[0] = new Vector3f(1,0,0);
		bases[1] = new Vector3f(0,1,0);
		bases[2] = new Vector3f(0,0,1);
		
		Vector3f[] rBases = new Vector3f[3];
		for(int i=0;i<3;i++){
			rBases[i] = new Vector3f();
			mat.transformAsPoint3Df(bases[i], rBases[i]);
		}
		
		float rxRad = (float) Math.atan2(rBases[1].Z, rBases[2].Z);
		float ryRad = (float) Math.atan2(-rBases[0].Z, Math.sqrt(Math.pow(rBases[1].Z, 2) + Math.pow(rBases[2].Z, 2)));
		float rzRad = (float) Math.atan2(rBases[0].Y, rBases[0].X);
		
		int rxDeg = (int) Math.round((double)rxRad*180.0/Math.PI);
		int ryDeg = (int) Math.round((double)ryRad*180.0/Math.PI);
		int rzDeg = (int) Math.round((double)rzRad*180.0/Math.PI);
		
		return new int[]{ rxDeg, ryDeg, rzDeg};
	}

	/**
	 * Used to translate the neuron structure in the X-Y 
	 * viewing plane. Unlike in mouse rotate, which
	 * simply passes the new parameters back to the dialog
	 * to carry out the transform, this one takes care of
	 * the translation by itself (since it is a fairly
	 * trivial endeavour). 
	 * @param tx
	 * @param ty
	 * @param zoom
	 */
	public void mouseTranslate(int tx, int ty, double zoom){
		for(int i=0;i<spacePts.size();i++){
			float[] pt = spacePts.get(i);
			pt[0] += tx*zoom;
			pt[1] += ty*zoom;
		}
		
		makeViewImage();
		
		//BitSet axonMask = highlightAxon(currentAxon);
		highlightAxon(currentAxon);
		//ViewJFrameImage frame = destImage.getParentFrame();
		
		//frame.getComponentImage().setPaintMask(axonMask);
		//frame.getControls().getTools().setOpacity(1.0f);
		//frame.getControls().getTools().setPaintColor(Color.RED);
		
		//frame.updateImages(true);
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
		mat.setZoom(zoom, zoom, zoom);
		
		
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
			rotJoint[0] += tx*zoom;
			rotJoint[1] += ty*zoom;
			
			spacePts.set(i, rotJoint);
			
		}
		
		makeViewImage();
		
		
		ViewJFrameImage frame = destImage.getParentFrame();
		
		highlightAxon(currentAxon);
		if(!showAxon){
			displayConvexHull();
		}
		/*if(showAxon){
			highlightAxon(currentAxon);
			//BitSet axonMask = highlightAxon(currentAxon);
			//frame.getComponentImage().setPaintMask(axonMask);
		}else{
			convexHull();
			//BitSet hullMask = convexHull();
			//frame.getComponentImage().setPaintMask(hullMask);
		}*/
		
		frame.updateImages(true);
	}
	
	public void showAxon(){
		showAxon = true;
		
	}
	
	public void showHull(){
		showAxon = false;
	}

	public void setUseLength(boolean useLength){
		axonUseLength = useLength;
	}
	
	/**
	 * This method handles the actual writing and calculation steps
	 * after the user has chosen an axon. Other than rearranging
	 * the order of forward connections, this portion is more or
	 * less the same as the back half of the sibling algorithm.
	 */
	public void write(){
		
		/*final Vector3f splitPt;
		if(branchDensity){
			splitPt = null;
		}else{
			splitPt = splitImage.getVOIs().get(0).exportPoint();
			int[] extents = srcImage.getExtents();
			int width = extents[0];
			int height = extents[1];
			int depth = extents[2];
			
			if(splitPt.X == width/2 && splitPt.Y == height/2
					&& splitPt.Z == depth/2){
				int response = JOptionPane.showConfirmDialog(null, "Split point has not moved. Is it correct?", "Continue?",
						JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
				if(response == JOptionPane.NO_OPTION)
					return false;
				
			}
		}*/
		setCompleted(false);
		
		final PlugInAlgorithm3DSWCViewer alg = this;
		
		Thread writeThread = new Thread(){
			public void run(){
				try{
					viewerOpen = false;
					//convexHullVolume = convexHullVolume(swcCoordinates, connections, false);
					
					int maxOrder;
					
					if(showViewer){
						calculateDistances(swcCoordinates);
						if(axonUseLength){
							maxOrder = determineOrder_useLength(swcCoordinates, connections, currentAxon);
						}else{
							maxOrder = determineOrder(swcCoordinates, connections, currentAxon);
						}
					}else{
						maxOrder = 0;
						for(int i=0;i<swcCoordinates.size();i++){
							int order = (int)swcCoordinates.get(i).get(0)[5];
							if(order > maxOrder)
								maxOrder = order;
						}
						
						currentAxon = tips.get(0);
						for(int i : tips){
							if(swcCoordinates.get(i).get(0)[5] == 1){
								currentAxon = i;
								break;
							}
						}
						
					}
					
					if(branchDensity){
						ArrayList<String> messages = consolidateFilaments(swcCoordinates, connections, maxOrder);
						float[] branchLengths = recalculateDistances(swcCoordinates, connections);
						addToMessages(swcCoordinates, messages);
						
						float hullVolume = convexHullVolume(swcCoordinates, connections, true);
						
						try{
							String output = exportStatsToCSV(swcCoordinates, connections, swcFile, messages, branchLengths, -1.0f, hullVolume, maxOrder);
							append("Exported stats to CSV -> " + output, blackText);
						} catch (IOException e) {
							append("Could not export stats to CSV for " + swcFile.getName(), redText);
						}
						
						try {
							String output = writeSWC(swcFile, swcCoordinates, messages, branchLengths);
							append("Converted to SWC -> " + output, blackText);
						} catch (IOException e) {
							append("Could not write SWC for " + swcFile.getName(), redText);
						}
						
						ArrayList<float[]> stats = branchDensity();
						
						append("Writing branch density information", blackText);
						//write stats out
						String parent = swcFile.getParent();
						String name = swcFile.getName();
						String output = parent + File.separator + "branch_density.csv";
						File outputFile = new File(output);
						
						FileWriter fw = new FileWriter(outputFile, true);
						
						fw.append(name + "\n");
						
						String[] rows = new String[]{"Distance,", "# of Branches,", "Branch Lengths,"};
						
						float distance = 5;
						float increment = 5;
						int num = 0;
						float lengths = 0;
						
						for(int i=0;i<stats.size();i++){
							float[] stat = stats.get(i);
							while(stat[0] > distance){
								rows[0] += distance + ",";
								rows[1] += num + ",";
								rows[2] += lengths + ",";
								distance += increment;
								num = 0;
								lengths = 0;
							}
							num += stat[1];
							lengths += stat[2];
						}
						
						rows[0] += distance;
						rows[1] += num;
						rows[2] += lengths;
						
						for(int i=0;i<3;i++){
							rows[i] += "\n";
							fw.append(rows[i]);
						}

						fw.close();	
						
						SimpleAttributeSet greenText = new SimpleAttributeSet(blackText);
						StyleConstants.setForeground(greenText, Color.green.darker());

						append("Finished writing stats and SWC files", greenText);
						append("-----------------------------------------", blackText);
					}else{

						float[] splitLoc = null;
						int filIndex = currentAxon;
						
						ArrayList<float[]> piece = swcCoordinates.get(currentAxon);
						float sDist = splitDist;
						int ind = piece.size()-1;
						while(sDist > 0){
							float[] fa = piece.get(ind);
							float[] fa2;
							if(ind == 0){
								int con = (int)fa[4];
								if(con == -1){
									append("Growth cone is longer than axon", redText);
									setCompleted(false);
									alg.notifyListeners(alg);
									return;
								}
								piece = swcCoordinates.get(con);
								filIndex = con;
								ind = piece.size()-1;
								fa2 = piece.get(ind);
							}else{
								fa2 = piece.get(ind - 1);
								ind--;
							}
							float dist = 0;
							for(int i=0;i<3;i++){
								float d = fa[i] - fa2[i];
								dist += d*d;
							}
							dist = (float)Math.sqrt(dist);
							if(sDist - dist > 0){
								sDist -= dist;
							}else{
								if(Math.abs(sDist) > Math.abs(sDist - dist)){
									splitLoc = fa2;
								}else{
									splitLoc = fa;
								}
								break;
							}
						}
						
						
						//splitImage.getParentFrame().removeWindowListener(alg);
						//splitImage.getParentFrame().close();

						int[] axonIndex = new int[1];
						ArrayList<ArrayList<float[]>> growthCone = filterGrowthCone(splitLoc, filIndex, axonIndex);
						
						/*for(int i=0;i<growthCone.size();i++){
							String parent = swcFile.getParent();
							File partFile = new File(parent + File.separator + "part_" + i + ".swc");
							FileWriter fw = new FileWriter(partFile);
							
							ArrayList<float[]> fil = growthCone.get(i);
							for(int j=0;j<fil.size();j++){
								float[] coord = fil.get(j);
								int c;
								if(j==0){
									c = -1;
								}else{
									c = j;
								}
								String line = String.format("%d %d %4.5f %4.5f %4.5f 0.1 %d \n", j+1, 2, coord[0], coord[1], coord[2], c);
								fw.append(line);
							}
							
							fw.close();
						}*/
						
						
						ArrayList<ArrayList<Integer>> gcConnections;
						if(disconnected){
							gcConnections = makeConnectionsTol(growthCone);
						}else{
							gcConnections = makeConnections(growthCone);
						}
						
						calculateDistances(growthCone);
						int gcOrder;
						if(axonUseLength){
							gcOrder = determineOrder_useLength(growthCone, gcConnections, axonIndex[0]);
						}else{
							gcOrder = determineOrder(growthCone, gcConnections, axonIndex[0]);
						}
						
						ArrayList<String> gcMessages = consolidateFilaments(growthCone, gcConnections, gcOrder);
						float[] gcLengths = recalculateDistances(growthCone, gcConnections);
						addToMessages(growthCone, gcMessages);
						
						/*float[][] tipPts = new float[vertexInd.length][];
						for(int i=0;i<vertexInd.length;i++){
							int index = vertexInd[i];
							ArrayList<float[]> fil = swcCoordinates.get(index);
							float[] tail = fil.get(fil.size()-1);
							tipPts[i] = tail;
						}
	
						ArrayList<Integer> vertexList = new ArrayList<Integer>();
						Hashtable<Integer, Integer> filMap = new Hashtable<Integer, Integer>();
						
						for(int i=0;i<growthCone.size();i++){
							ArrayList<float[]> fil = growthCone.get(i);
							float[] tail = fil.get(fil.size()-1);
							for(int j=1;j<tipPts.length;j++){
								//int ind = vertexInd[j];
								float[] target = tipPts[j];
								if(tail[0] == target[0] && tail[1] == target[1] && tail[2] == target[2]){
									vertexList.add(i);
									filMap.put(j, vertexList.size());
									break;
								}
							}
						}
						
						filMap.put(0, 0);
						
						//vertexList is list of points in growth cone that are in the hull
						
						int[] gcVertexInd = new int[vertexList.size()+1];
						gcVertexInd[0] = 0; //Redundant 
						for(int i=0;i<vertexList.size();i++){
							gcVertexInd[i+1] = vertexList.get(i);
						}
						//fixing faceVerticies for the growth cone will be difficult
						
						ArrayList<int[]> faces = new ArrayList<int[]>();
						for(int i=0;i<faceVerticies.length;i++){
							int[] face = faceVerticies[i];
							int[] outFace = new int[face.length];
							System.arraycopy(face, 0, outFace, 0, face.length);
							int numNull = 0;
							int nullInd = -1;
							for(int j=0;j<face.length;j++){
								int vertex = face[j];
								if(vertex == 0 || filMap.get(vertex) == null){
									numNull++;
									nullInd = j;
								}else{
									outFace[j] = filMap.get(vertex);
								}
							}
							
							if(numNull == 0){
								faces.add(outFace);
							}else if(numNull == 1){
								outFace[nullInd] = 0;
								faces.add(outFace);
							}
						}
						
						int[][] gcFaceVerticies = new int[faces.size()][];
						for(int i=0;i<faces.size();i++){
							gcFaceVerticies[i] = faces.get(i);
						}
						
						//This doesn't seem to
						//float gcHullVolumeN = convexHullVolumeNew(growthCone, gcVertexInd, gcFaceVerticies);
						//System.out.println(gcHullVolumeN);
						 
						 */
						float gcHullVolume = convexHullVolume(growthCone, gcConnections, true);
						
						PlugInAlgorithmSWCVolume alg = new PlugInAlgorithmSWCVolume(srcImage, growthCone);
						alg.run();
						
						String parent = swcFile.getParent();
						String name = swcFile.getName();
						String sub = name.substring(0, name.lastIndexOf("."));
						String ext = name.substring(name.lastIndexOf("."));
						File inFile = new File(parent + File.separator + sub + "_gc" + ext);
						
						try{
							append("Calculating volumes", blackText);
							String output = exportStatsToCSV(growthCone, gcConnections, inFile, gcMessages, gcLengths, alg.getVolume(), gcHullVolume, gcOrder);
							append("Exported stats to CSV -> " + output, blackText);
						} catch (IOException e) {
							append("Could not export stats to CSV for " + swcFile.getName(), redText);
						}
						
						try {
							String output = writeSWC(inFile, growthCone, gcMessages, gcLengths);
							append("Converted to SWC -> " + output, blackText);
						} catch (IOException e) {
							append("Could not write SWC for " + swcFile.getName(), redText);
						}

						SimpleAttributeSet greenText = new SimpleAttributeSet(blackText);
						StyleConstants.setForeground(greenText, Color.green.darker());

						append("Finished writing stats and SWC files", greenText);
						append("-----------------------------------------", blackText);

					}
					setCompleted(true);


				}catch(Exception e){
					append("The following Java error has occured:", redText);
					append(e.toString(), redText);
					for(StackTraceElement t : e.getStackTrace())
						append(t.toString(), redText);
				}


				alg.notifyListeners(alg);
			}
		};
		
		writeThread.start();
	}
	
	/*public boolean isTipInHullOld(int row){
		
		if(verticies == null)
			return false;
		
		int branch = tips.get(row);
		float[] spacePt = spacePts.get(branch+1);
		Point3d pt = new Point3d(spacePt[0], spacePt[1], spacePt[2]);
		for(int i=0;i<verticies.length;i++){
			Point3d vPt = verticies[i];
			if(vPt.x == pt.x && vPt.y == pt.y && vPt.z == pt.z){
				return true;
			}
		}
		
		return false;
	}*/
	
	public boolean isTipInHull(int row){
		
		if(vertexInd == null)
			return false;
		
		int branch = tips.get(row);
		for(int i=0;i<vertexInd.length;i++){
			if(branch == vertexInd[i])
				return true;
		}
		
		return false;
	}
	
	private int calculateConvexHull(ArrayList<ArrayList<float[]>> swcCoordinates, ArrayList<Integer> tips, int[][]faceVerticies, int[] vertexInd){
		
		ArrayList<Point3d> ptList = new ArrayList<Point3d>();
		float[] originPt = swcCoordinates.get(0).get(0);
		Point3d originPt3d = new Point3d(originPt[0], originPt[1], originPt[2]);
		ptList.add(originPt3d);
		
		for(int i : tips){
			ArrayList<float[]> fil = swcCoordinates.get(i);
			float[] pt = fil.get(fil.size()-1);
			Point3d pt3d = new Point3d(pt[0], pt[1], pt[2]);
			
			//Added this if statement too, need to remove
			//if(i == currentAxon)
			//	tipPt = pt3d; 
			ptList.add(pt3d);
		}
		
		Point3d[] pts = new Point3d[ptList.size()];
		ptList.toArray(pts);
		
		QuickHull3D hull = new QuickHull3D(pts);
		
		Point3d[] verticies = hull.getVertices();
		int[][] faceVerticiesA = hull.getFaces();
		for(int i=0;i<faceVerticiesA.length;i++){
			faceVerticies[i] = faceVerticiesA[i];
		}
		
		int cnt = 0;
		for(int i=1;i<verticies.length;i++){
			Point3d vPt = verticies[i];
			Point3d lPt = ptList.get(cnt);
			while(vPt.x != lPt.x || vPt.y != lPt.y || vPt.z != lPt.z){
				cnt++;
				if(cnt >= ptList.size()){//Something messed up
					System.out.println("Something got jacked up with convex hull");
					break;
				}
				lPt = ptList.get(cnt);
			}
			vertexInd[i] = tips.get(cnt-1);
		}
		
		return verticies.length;
	}
	
	private void displayConvexHull(){
		
		for(int i=0;i<faceVerticies.length;i++){
			for(int j=0;j<faceVerticies[i].length;j++){
				
				int j1 = j+1;
				if(j1==faceVerticies[i].length)
					j1 = 0;
				int ind1 = faceVerticies[i][j];
				int ind2 = faceVerticies[i][j1];
				float[] pt0f;
				float[] pt1f;
				if(ind1 == 0){
					pt0f = spacePts.get(0);
					pt1f = spacePts.get(vertexInd[ind2] + 1);
				}else if(ind2 == 0){
					pt0f = spacePts.get(vertexInd[ind1] + 1);
					pt1f = spacePts.get(0);
				}else{
					pt0f = spacePts.get(vertexInd[ind1] + 1);
					pt1f = spacePts.get(vertexInd[ind2] + 1);
				}
				//Point3d pt03d = verticies[ind1];
				//Point3d pt13d = verticies[ind2];
				
				Point pt0 = new Point((int)pt0f[0], (int)pt0f[1]);
				Point pt1 = new Point((int)pt1f[0], (int)pt1f[1]);
				
				ArrayList<Point> line = bresenham(pt0, pt1);
	
				for(int k=0;k<line.size();k++){
					Point pt = line.get(k);
					if(pt.x > 0 && pt.x < 512 &&
							pt.y > 0 && pt.y < 512){
						//hullMask.set(pt.x + pt.y*512);
						//hullMask[pt.x + pt.y*512] = (byte) 255;
						destImage.setC(pt.x + pt.y*512, 2, 255);
					}
				}
			}
		}
	}
	/**
	 * No longer being used, split into two methods
	 */
	@SuppressWarnings("unused")
	private void convexHull(){
		
		//Added line, should remove
		//Point3d tipPt = null;
		
		ArrayList<Point3d> ptList = new ArrayList<Point3d>();
	
		//BitSet hullMask = new BitSet(512*512);
		//byte[] hullMask = new byte[512*512];
		
		//tips.add(0, -1);
		
		float[] originPt = spacePts.get(0);
		Point3d originPt3d = new Point3d(originPt[0], originPt[1], originPt[2]);
		ptList.add(originPt3d);
		
		for(int i : tips){
			float[] pt = spacePts.get(i+1);
			Point3d pt3d = new Point3d(pt[0], pt[1], pt[2]);
			
			//Added this if statement too, need to remove
			//if(i == currentAxon)
			//	tipPt = pt3d; 
			ptList.add(pt3d);
		}
		
		Point3d[] pts = new Point3d[ptList.size()];
		ptList.toArray(pts);
		
		QuickHull3D hull = new QuickHull3D(pts);
		
		Point3d[] verticies = hull.getVertices();
		faceVerticies = hull.getFaces();
		
		vertexInd = new int[verticies.length];
		
		int cnt = 0;
		for(int i=1;i<verticies.length;i++){
			Point3d vPt = verticies[i];
			Point3d lPt = ptList.get(cnt);
			while(vPt.x != lPt.x || vPt.y != lPt.y || vPt.z != lPt.z){
				cnt++;
				if(cnt >= ptList.size()){//Something messed up
					System.out.println("Something got jacked up with convex hull");
					break;
				}
				lPt = ptList.get(cnt);
			}
			vertexInd[i] = tips.get(cnt-1);
		}
		
		/**/
		/*
		boolean onHull = false;
		for(int i=0;i<verticies.length;i++){
			Point3d pt = verticies[i];
			if(pt.x == tipPt.x && pt.y == tipPt.y && pt.z == tipPt.z){
				onHull = true;
				break;
			}
		}
		
		if(!onHull){
			
			ArrayList<float[]> fil = swcCoordinates.get(currentAxon);
			int connection = (int)fil.get(0)[4];
	
			float[] start = spacePts.get(connection+1);
			float[] tip = spacePts.get(currentAxon+1);
			
			Vector3f headPt = new Vector3f(tip[0], tip[1], tip[2]);
			Vector3f vecC = new Vector3f(start[0], start[1], start[2]);
			vecC = Vector3f.sub(headPt, vecC);
			
			float minDist = Float.MAX_VALUE;
			int minInd = -1;
			
			for(int i=0;i<faceVerticies.length;i++){
				int[] vert = faceVerticies[i];
				Point3d ptA = verticies[vert[0]];
				Point3d ptB = verticies[vert[1]];
				Point3d ptC = verticies[vert[2]];
				
				Vector3f originPt = new Vector3f((float)ptA.x, (float)ptA.y, (float)ptA.z);
				Vector3f vecA = new Vector3f((float)ptB.x, (float)ptB.y, (float)ptB.z);
				Vector3f vecB = new Vector3f((float)ptC.x, (float)ptC.y, (float)ptC.z);
				
				float distance = distanceVectorToPlane(originPt, vecA, vecB, vecC, headPt);
				
				if(distance > 0 && distance < minDist){
					minDist = distance;
					minInd = i;
				}
				
			}
			
			int i = minInd;
			
			for(int j=0;j<faceVerticies[i].length;j++){
				
				int j1 = j+1;
				if(j1==faceVerticies[i].length)
					j1 = 0;
				int ind1 = faceVerticies[i][j];
				int ind2 = faceVerticies[i][j1];
				Point3d pt03d = verticies[ind1];
				Point3d pt13d = verticies[ind2];
				
				Point pt0 = new Point((int)pt03d.x, (int)pt03d.y);
				Point pt1 = new Point((int)pt13d.x, (int)pt13d.y);
				
				ArrayList<Point> line = bresenham(pt0, pt1);
	
				for(int k=0;k<line.size();k++){
					Point pt = line.get(k);
					if(pt.x > 0 && pt.x < 512 &&
							pt.y > 0 && pt.y < 512)
						hullMask.set(pt.x + pt.y*512);
				}
			}
		}
		
		hullMask.or(highlightAxon(currentAxon));
		*/
		/**/
		
		for(int i=0;i<faceVerticies.length;i++){
			for(int j=0;j<faceVerticies[i].length;j++){
				
				int j1 = j+1;
				if(j1==faceVerticies[i].length)
					j1 = 0;
				int ind1 = faceVerticies[i][j];
				int ind2 = faceVerticies[i][j1];
				Point3d pt03d = verticies[ind1];
				Point3d pt13d = verticies[ind2];
				
				Point pt0 = new Point((int)pt03d.x, (int)pt03d.y);
				Point pt1 = new Point((int)pt13d.x, (int)pt13d.y);
				
				ArrayList<Point> line = bresenham(pt0, pt1);
	
				for(int k=0;k<line.size();k++){
					Point pt = line.get(k);
					if(pt.x > 0 && pt.x < 512 &&
							pt.y > 0 && pt.y < 512){
						//hullMask.set(pt.x + pt.y*512);
						//hullMask[pt.x + pt.y*512] = (byte) 255;
						destImage.setC(pt.x + pt.y*512, 2, 255);
					}
				}
			}
		}
		
		/*try{
			destImage.importRGBData(2, 0, hullMask, true);
		}catch(IOException e){
			e.printStackTrace();
		}*/
		
		//return hullMask;
		
	}

	/**
	 * Convex hull volume for non-consolidated filaments
	 * 
	 * @param swcCoordinates
	 * @param connections
	 * @return
	 */
	private float convexHullVolume(ArrayList<ArrayList<float[]>> swcCoordinates, ArrayList<ArrayList<Integer>> connections, boolean consolidated){
		
		ArrayList<Point3d> ptList = new ArrayList<Point3d>();
		
		float[] origin = swcCoordinates.get(0).get(0);
		ptList.add(new Point3d(origin[0], origin[1], origin[2]));
		
		if(consolidated){
			for(int i=0;i<swcCoordinates.size();i++){
				ArrayList<float[]> fil = swcCoordinates.get(i);
				float[] pt = fil.get(fil.size()-1);
				ptList.add(new Point3d(pt[0], pt[1], pt[2]));
			}
		}else{
			//This portion is for non-consolidated only
			ArrayList<Integer> tips = new ArrayList<Integer>();
			for(int i=0;i<connections.size();i++){
				ArrayList<Integer> branches = connections.get(i);
				if(branches.size()==0){
					tips.add(i);
				}
			}
			
			for(int i : tips){
				ArrayList<float[]> fil = swcCoordinates.get(i);
				float[] pt = fil.get(fil.size()-1);
				ptList.add(new Point3d(pt[0], pt[1], pt[2]));
			}
		}
		
		float volume = 0;
		
		Point3d[] pts = new Point3d[ptList.size()];
		ptList.toArray(pts);
		
		QuickHull3D hull = new QuickHull3D(pts);
		
		Point3d centroid = new Point3d();
		
		for(int i=0;i<pts.length;i++){
			Point3d pt = pts[i];
			centroid.add(pt);
		}
		
		double num = pts.length;
		centroid.x /= num;
		centroid.y /= num;
		centroid.z /= num;
		
		Point3d[] verticies = hull.getVertices();
		int[][] faceVerticies = hull.getFaces();
		
		for(int i=0;i<faceVerticies.length;i++){
			
			Point3d ptA = new Point3d(verticies[faceVerticies[i][0]]);
			Point3d ptB = new Point3d(verticies[faceVerticies[i][1]]);
			Point3d ptC = new Point3d(verticies[faceVerticies[i][2]]);
			Point3d ptCentroid = new Point3d(centroid);
			//Point3d ptCentroid = new Point3d(boxVec.X, boxVec.Y, boxVec.Z);
			
			ptB.sub(ptA);
			ptC.sub(ptA);
			ptCentroid.sub(ptA);
			
			Vector3f vecA = new Vector3f((float)ptB.x, (float)ptB.y, (float)ptB.z);
			Vector3f vecB = new Vector3f((float)ptC.x, (float)ptC.y, (float)ptC.z);
			Vector3f vecC = new Vector3f((float)ptCentroid.x, (float)ptCentroid.y, (float)ptCentroid.z);
			Vector3f axb = Vector3f.cross(vecA, vecB);
			
			volume += Math.abs(axb.dot(vecC));
			//System.out.println(Math.abs(axb.dot(vecC)));
		}
		
		volume /= 6.0f;
		
		return volume;
	}
	
	private float convexHullVolumeNew(ArrayList<ArrayList<float[]>> swcCoordinates, int[] vertexInd, int[][] faceVerticies){
		
		float volume = 0;
		
		Point3d[] verticies = new Point3d[vertexInd.length];
		float[] origin = swcCoordinates.get(0).get(0);
		verticies[0] = new Point3d(origin[0], origin[1], origin[2]);
		
		for(int i=1;i<vertexInd.length;i++){
			int index = vertexInd[i];
			ArrayList<float[]> fil = swcCoordinates.get(index);
			float[] tipPt = fil.get(fil.size()-1);
			verticies[i] = new Point3d(tipPt[0], tipPt[1], tipPt[2]);
		}
		
		Point3d centroid = new Point3d();
		for(int i=0;i<verticies.length;i++){
			Point3d pt = verticies[i];
			centroid.add(pt);
		}
		
		double num = verticies.length;
		centroid.x /= num;
		centroid.y /= num;
		centroid.z /= num;
		
		for(int i=0;i<faceVerticies.length;i++){
			
			Point3d ptA = new Point3d(verticies[faceVerticies[i][0]]);
			Point3d ptB = new Point3d(verticies[faceVerticies[i][1]]);
			Point3d ptC = new Point3d(verticies[faceVerticies[i][2]]);
			Point3d ptCentroid = new Point3d(centroid);
			
			ptB.sub(ptA);
			ptC.sub(ptA);
			ptCentroid.sub(ptA);
			
			Vector3f vecA = new Vector3f((float)ptB.x, (float)ptB.y, (float)ptB.z);
			Vector3f vecB = new Vector3f((float)ptC.x, (float)ptC.y, (float)ptC.z);
			Vector3f vecC = new Vector3f((float)ptCentroid.x, (float)ptCentroid.y, (float)ptCentroid.z);
			Vector3f axb = Vector3f.cross(vecA, vecB);
			
			volume += Math.abs(axb.dot(vecC));
		}
		
		volume /= 6.0f;
		
		return volume;
	}

	/**
	 * Using the selected branch, create a bitset to be used
	 * as the image mask that overlays both the branch that
	 * was selected as well as the connections all the way
	 * back to the origin. 
	 * @param branch
	 * @return
	 */
	private void highlightAxon(int branch){
		
		currentAxon = branch;
		
		//BitSet axonMask = new BitSet(512*512);
		//byte[] axonMask = new byte[512*512];
		//byte[] blank = new byte[512*512];
		
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
						pt.y > 0 && pt.y < 512){
					//axonMask.set(pt.x + pt.y*512);
					//axonMask[pt.x + pt.y*512] = (byte) 255;
					destImage.setC(pt.x + pt.y*512, 2, 0);
					destImage.setC(pt.x + pt.y*512, 3, 0);
				}
			}
		}
		
		/*try{
			destImage.importRGBData(1, 0, axonMask, true);
			destImage.importRGBData(2, 0, blank, true);
			destImage.importRGBData(3, 0, blank, true);
		}catch(IOException e){
			e.printStackTrace();
		}*/
		
		//if(!branchDensity)
		//	highlightAxonSplit();
		
		//return axonMask;
	}

	private ArrayList<float[]> branchDensity(){
		//Would be based on the consolidated branches, so comes after that method
		//Could reuse the messages to figure out where the branches fall
		
		//Get the indicies of the first order branches
		ArrayList<Integer> indicies = new ArrayList<Integer>();
		for(int i=1;i<swcCoordinates.size();i++){
			ArrayList<float[]> fil = swcCoordinates.get(i);
			if(fil.get(0)[5] == 2){
				indicies.add(i);
			}
		}
		
		ArrayList<float[]> axon = swcCoordinates.get(0);
		float axonLength = axon.get(axon.size()-1)[3];
		
		ArrayList<float[]> stats = new ArrayList<float[]>();
		for(int i=indicies.size()-1;i>=0;i--){
			int ind = indicies.get(i);
			int until;
			if(i == indicies.size()-1){
				until = swcCoordinates.size();
			}else{
				until = indicies.get(i+1);
			}
			float axonDistance = axonLength - swcCoordinates.get(ind).get(0)[3];
			int numBranches = until - ind;
			float branchLengths = 0;
			for(int j=ind;j<until;j++){
				ArrayList<float[]> fil = swcCoordinates.get(j);
				branchLengths += fil.get(fil.size()-1)[3];
			}
			
			float[] branchStat = new float[]{axonDistance, (float) numBranches, branchLengths};
			stats.add(branchStat);
			
		}
		
		return stats;
	}
	
	private ArrayList<ArrayList<float[]>> filterGrowthCone(float[] splitPt, int filIndex, int[] axonIndex){
		ArrayList<ArrayList<float[]>> growthCone = new ArrayList<ArrayList<float[]>>();
		ArrayDeque<Integer> indexStack = new ArrayDeque<Integer>();
		
		ArrayList<float[]> fil = swcCoordinates.get(filIndex);
		ArrayList<float[]> addFil = new ArrayList<float[]>();
		boolean add = false;
		for(int i=0;i<fil.size();i++){
			float[] pt = fil.get(i);
			if(splitPt == pt)
				add = true;
			if(add){
				float[] newPt = new float[pt.length];
				System.arraycopy(pt, 0, newPt, 0, pt.length);
				addFil.add(newPt);
			}
		}
		
		growthCone.add(addFil);
		
		ArrayList<Integer> forward = connections.get(filIndex);
		for(int i=forward.size()-1;i>=0;i--){
			indexStack.addFirst(forward.get(i));
		}
		
		while(!indexStack.isEmpty()){
			int index = indexStack.poll();
			if(index == currentAxon)
				axonIndex[0] = growthCone.size();
			fil = swcCoordinates.get(index);
			addFil = new ArrayList<float[]>();
			for(int i=0;i<fil.size();i++){
				float[] pt = fil.get(i);
				float[] newPt = new float[pt.length];
				System.arraycopy(pt, 0, newPt, 0, pt.length);
				addFil.add(newPt);
			}
			growthCone.add(addFil);
			forward = connections.get(index);
			for(int i=forward.size()-1;i>=0;i--){
				indexStack.addFirst(forward.get(i));
			}
		}
		
		return growthCone;
	}
	
	/*private void highlightAxonSplit(){
		
		ViewJFrameImage frame = splitImage.getParentFrame();
		int width = srcImage.getWidth(0);
		int height = srcImage.getHeight(0);
		
		int branch = currentAxon;
		
		if(!showViewer){
			for(int i=0;i<tips.size();i++){
				int tipNum = tips.get(i);
				ArrayList<float[]> fil = swcCoordinates.get(tipNum);
				if(fil.get(0)[5] == 1){
					branch = tipNum;
					break;
				}
			}
		}
		
		ArrayList<Point> axonPts = new ArrayList<Point>();
		
		float[] origin = srcImage.getOrigin();
		float[] res = srcImage.getResolutions(0);
		
		while(branch > -1){
			ArrayList<float[]> fil = swcCoordinates.get(branch);
			for(int i = fil.size()-1;i>=0;i--){
				float[] coord = fil.get(i);
				float x = (coord[0]-origin[0])/res[0];
				float y = (coord[1]-origin[1])/res[1];
				Point pt = new Point((int)x, (int)y);
				axonPts.add(pt);
			}
			branch = (int)fil.get(0)[4];
		}
		
		BitSet axonMask = new BitSet(width*height);
		
		for(int i=0;i<axonPts.size()-1;i++){
			Point oPt = axonPts.get(i);
			Point nPt = axonPts.get(i+1);
			ArrayList<Point> linePts = bresenham(oPt, nPt);
			for(int j=0;j<linePts.size();j++){
				Point pt = linePts.get(j);
				axonMask.set(pt.x + pt.y * width);
			}
		}
		
		frame.getComponentImage().setPaintMask(axonMask);
		
		frame.updateImages(true);
		
	}*/
	
	/*private void setupSplitChooser(){
		
		int[] extents = srcImage.getExtents();
		int width = extents[0];
		int height = extents[1];
		int depth = extents[2];
		
		AlgorithmMaximumIntensityProjection proj = new AlgorithmMaximumIntensityProjection(srcImage, 0, depth-1, depth,
				srcImage.getMin(), srcImage.getMax(), true, true, AlgorithmMaximumIntensityProjection.Z_PROJECTION);
		proj.run();
		splitImage = proj.getResultImage().get(0);
		
		ViewJFrameImage frame = new ViewJFrameImage(splitImage);
		VOI voi = new VOI((short)0, "Split Point", VOI.POINT, -1.0f);
		
		Vector3f centerPt = new Vector3f(width/2, height/2, 0);
		VOIPoint voiPt = new VOIPoint(VOI.POINT, centerPt);
		voiPt.setLabel("Split point");
		voi.importCurve(voiPt);
		
		frame.setVisible(true);
		
		splitImage.getVOIs().add(voi);
		splitImage.notifyImageDisplayListeners(true, null);
		
		srcImage.getVOIs().add(voi);
		
		currentAxon = tips.get(0);
		
		if(!showViewer){
			calculateDistances(swcCoordinates);
			if(axonUseLength){
				determineOrder_useLength(swcCoordinates, connections, currentAxon);
			}else{
				determineOrder(swcCoordinates, connections, currentAxon);
			}
			
		}
		
		highlightAxonSplit();
		
		frame.getControls().getTools().setOpacity(1.0f);
		frame.getControls().getTools().setPaintColor(Color.RED);
		
		if(showViewer){
			frame.removeWindowListener(frame);
		}else{
			frame.addWindowListener(this);
		}

		
	}*/
	
	//Code right now is in the convexHull() method
	private float distanceVectorToPlane(Vector3f originPt, Vector3f vecA, Vector3f vecB, Vector3f vecC, Vector3f headPt){
		
		Vector3f a = Vector3f.sub(vecA, originPt);
		Vector3f b = Vector3f.sub(vecB, originPt);
		Vector3f pt = Vector3f.sub(headPt, originPt);
		//Vector vecC is already relative since it contains direction information
		
		Vector3f d = Vector3f.cross(a, b);
		float num = d.dot(pt);
		float denom = d.dot(vecC);
		
		if(denom == 0){
			return -1.0f;
		}
		
		float magC = vecC.length();
		float distance = num * magC / denom;
		
		if(distance < 0){
			return -distance;
		}else{
			return -1.0f;
		}
		
	}

	/**
	 * Adds the branch length and distance along the
	 * axon/parent to the output messages. 
	 * @param messages
	 */
	private void addToMessages(ArrayList<ArrayList<float[]>> swcCoordinates, ArrayList<String> messages){
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

	/**
	 * Determines the length of each individual filament
	 * read from the Imaris trace. These will be used
	 * to determine the axon filaments. 
	 */
	private void calculateDistances(ArrayList<ArrayList<float[]>> swcCoordinates){
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
				currPt[3] = 0;
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
	 * Consolidate the multitude of filaments from the input to make
	 * stat tracking easier. Also organizes the output SWC so that
	 * branch numbers are logical and go from beggining of the axon
	 * to the end of the axon.
	 * @param connections
	 * @param maxOrder
	 * @return
	 */
	private ArrayList<String> consolidateFilaments(ArrayList<ArrayList<float[]>> swcCoordinates, ArrayList<ArrayList<Integer>> connections, int maxOrder){
		
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
		
		swcCoordinates.clear();
		swcCoordinates.addAll(newFilaments);
		
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
	private int determineOrder(ArrayList<ArrayList<float[]>> swcCoordinates, ArrayList<ArrayList<Integer>> connections, int branch){
		
		if(showViewer)
			rearrangeBranches(branch);
		
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
	
	/**
	 * Three pass process to determine branch ordering and which filaments
	 * are the axon. Determines by finding the longest path from the first
	 * filament. 
	 * 
	 * Changed name to longest length to allow for axon to determined
	 * by either longest length or by filament ordering
	 * 
	 * @param connections
	 */
	
	private int determineOrder_useLength(ArrayList<ArrayList<float[]>> swcCoordinates, ArrayList<ArrayList<Integer>> connections, int branch){
		
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
		
		 
		// Pass 1
		// Accumulate the length of each filament. At the head
		// of the filament, store the cumulative length for 
		// later use. Once a tip has been reached, put the 
		// length to that tip in a tree map.
		// 
		// Pretty much a BFS as the ArrayDeque is used as a queue
		 
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
		
		
		// Pass 2
		// Tree map is based on length to the tips. Working
		// backwards from the furthest points to the closest
		// points should increase efficiency. 
		// 
		// Trace back from the tips and replace the tail
		// length values to the length at the tip. If you
		// are tracing backwards and the connected filament
		// already has been reached by a longer filament 
		// (which should always be the case because of the
		// tree map), then move on to the next tip.
		  
		// Rearrange forward connections so that the first one
		// in the list is the longest path away from this 
		// filament.
		
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
		
		if(showViewer)
			rearrangeBranches(branch);
		
		// Pass 3
		// Forward connections are organized so that the longest 
		// path is the first element. Increment the branch order
		// of all the other elements in the list. Traverse the
		// entire neuron in a BFS. 
		 
		
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

	private String exportStatsToCSV(ArrayList<ArrayList<float[]>> swcCoordinates, ArrayList<ArrayList<Integer>> connections,
			File file, ArrayList<String> messages, float[] branchLengths, float neuronVolume, float hullVolume, int maxOrder) throws IOException{
		String parent = file.getParent();
		String name = file.getName();
		name = name.substring(0, name.lastIndexOf("."));
		String output = parent + File.separator + name + "_stats.csv";
		File outputFile = new File(output);
		
		FileWriter fw = new FileWriter(outputFile);
		
		fw.append("Units," + resolutionUnit + "\n");
		
		//Write the new branch info here
		
		writeBranchInformation(swcCoordinates, connections, fw, neuronVolume, hullVolume, maxOrder);
		
		/*String branchInfo = "";
		branchInfo += "Total branch length," + String.valueOf(branchLengths[0]) + "\n";
		branchInfo += "Minus axon," + String.valueOf(branchLengths[1]) + "\n\n";
		
		fw.append(branchInfo);*/
		
		String header = "Branch Number,Branch Order,Branch Length,Length along parent \n";
		
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
	
	private String formatSWCLine(int lineNum, float[] line) {
		String format = "%d %d %4.5f %4.5f %4.5f %4.2f %d \n";
		int type;
		if(line[5] == 1.0F){
			type = 2;
		}else{
			type = 3;
		}
		
		return String.format(format, lineNum, type, line[0], line[1], line[2], line[6], (int)line[4]);
	}

	/**
	 * Build both forward and backwards connections based on
	 * the coordinates read from the Imaris trace. The 
	 * backwards connection routine is taken from the 
	 * Drosophila Registration algorithm written by
	 * Nish Pandya. 
	 * @return
	 */
	private ArrayList<ArrayList<Integer>> makeConnections(ArrayList<ArrayList<float[]>> swcCoordinates){
		
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
	private ArrayList<ArrayList<Integer>> makeConnectionsTol(ArrayList<ArrayList<float[]>> swcCoordinates){
		
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
	 * Draws all the branches on the image based on
	 * the transform variables made in the earlier
	 * portions. 
	 */
	private void makeViewImage(){
		
		
		//BitSet skeleton = new BitSet(512*512);
		byte[] skeleton = new byte[512*512];
		
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
					//skeleton.set(pt.x + pt.y*512);
					skeleton[pt.x + pt.y*512] = (byte) 255;
			}
		}
		
		try {
			//destImage.importData(0, skeleton, true);
			for(int i=0;i<4;i++){
				destImage.importRGBData(i, 0, skeleton, true);
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		
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
							 * Include in Volume Calculations (1=Include, -1=Exclude) (7)
							 */
							float[] coords = {coord_x,coord_y,coord_z,0,Float.NEGATIVE_INFINITY, 0f, -1.0f, -1.0f};
							
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

	/**
	 * Using the selected branch, make sure that it comes first
	 * in the forward connections. 
	 */
	private void rearrangeBranches(int branch){
		
		ArrayList<float[]> fil = swcCoordinates.get(branch);
		int change = branch;
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
	 * Recalculate distances for the consolidated branches
	 * so that it also includes the branch length and
	 * distance along the axon/parent this branch
	 * originates from. 
	 */
	private float[] recalculateDistances(ArrayList<ArrayList<float[]>> swcCoordinates, ArrayList<ArrayList<Integer>> connections){
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
			float[] fPt2 = new float[]{x, y, z};
			joints.set(i, fPt);
			spacePts.add(fPt2);
		}
		
		makeViewImage();
		
		currentAxon = tips.get(0);
		
	}
	
	private void writeBranchInformation(ArrayList<ArrayList<float[]>> swcCoordinates,
			ArrayList<ArrayList<Integer>> connections, FileWriter fw, float neuronVolume, float hullVolume, int maxOrder) throws IOException{
		
		float[] lengths = new float[maxOrder];
		for(int i=0;i<lengths.length;i++){
			lengths[i] = 0.0F;
		}
		
		for(int i=1;i<swcCoordinates.size();i++){
			ArrayList<float[]> fil = swcCoordinates.get(i);
			float filLength = fil.get(fil.size()-1)[3];
			int order = (int) fil.get(0)[5];
			lengths[order-1] += filLength;
		}
		
		float allBranches = 0;
		float higherOrder = 0;
		
		for(int i=1;i<maxOrder;i++){
			allBranches += lengths[i];
			if(i!=1)
				higherOrder += lengths[i];
		}
		
		if(neuronVolume >= 0){
			fw.append("\nVolumes\n");
			fw.append("Neuron volume," + neuronVolume + "\n");
		}
		
		fw.append("Convex hull volume," + hullVolume + "\n\n");
		fw.append("Branch lengths\n");
		fw.append("Total Branches," + String.valueOf(allBranches) + "\n");
		fw.append("Higher order," + String.valueOf(higherOrder) + "\n\n");
		
		for(int i=1;i<maxOrder;i++){
			fw.append("Order " + String.valueOf(i) + "," + String.valueOf(lengths[i]) + "\n");
		}
		
		fw.append("\n");
		
	}

	private String writeSWC(File file, ArrayList<ArrayList<float[]>> swcCoordinates, ArrayList<String> messages, float[] branchLengths) throws IOException{
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
	
	@Override
	public void windowClosed(WindowEvent e) {
		append("Canceling...", blackText);
		append("-----------------------------------------", blackText);
		setCompleted(true);
		notifyListeners(this);
	}
	public void windowOpened(WindowEvent e) {}
	@Override
	public void windowClosing(WindowEvent e) {}
	@Override
	public void windowIconified(WindowEvent e) {}
	@Override
	public void windowDeiconified(WindowEvent e) {}
	@Override
	public void windowActivated(WindowEvent e) {}
	@Override
	public void windowDeactivated(WindowEvent e) {}

}
