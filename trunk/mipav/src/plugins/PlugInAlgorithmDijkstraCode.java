import java.io.IOException;
import java.util.Arrays;
import java.util.BitSet;
import java.util.PriorityQueue;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;

/**
 * A modified Dijkstra's algorithm. Instead of using edge weights, a
 * cost image is passed into the algorithm in order to determine the
 * path, which doesn't change the algorithm that much, but provides
 * slightly different functionality. Can easily be changed to edge
 * weights if need be though.
 * 
 * Uses <code> PriorityQueue </code> for faster runtime. Paths are
 * usually short for the neuron segmentation, so not that big of a 
 * deal
 */

public class PlugInAlgorithmDijkstraCode extends AlgorithmBase {
	
	public static final int COST = 0;
	
	public static final int EDGE = 1;
	
	private int mode;
	
	private int[] costs;
	
	private int[] extents;
	
	private boolean[] visited;

	private int[] direction;
	
	private BitSet shortest;	
	
	private BitSet included;
	
	private int width;
	
	private int height;
	
	private int length;
	
	private int source;
	
	private PriorityQueue<Path> dijkstra;
	
	/**
	 * The original constructor for the accompanying dialog. Uses
	 * a <code>ModelImage</code> for the input target image.
	 * 
	 * @param costImage the cost to travel to a certain pixel, used to
	 * determine the shortest path
	 * @param visitedImage the targets for the Dijkstra algorithm. Once
	 * the path intersects ANY of the points in this image, the method 
	 * ends.
	 * @param start the seed point from which to determine the path to 
	 * points on the visitedImage
	 */

	public PlugInAlgorithmDijkstraCode(ModelImage costImage, ModelImage visitedImage, int start){
		
		super(null, costImage);

		extents = costImage.getExtents();
		width = extents[0];
		height = extents[1];
		length = width*height;
		
		//destImage = new ModelImage(ModelImage.BOOLEAN, extents, "Shortest Path");
		
		costs = new int[length];
		direction = new int[length];
		visited = new boolean[length];
		included = new BitSet(length);
		shortest = new BitSet(length);
		Arrays.fill(direction, -1);
		
		source = start;
		dijkstra = new PriorityQueue<Path>();
		mode = 0;
		
		try {
			costImage.exportData(0, length, costs);
			visitedImage.exportData(0, length, included);
		} catch (IOException e) {
			e.printStackTrace();
		}
		
	}
	
	/**
	 * The secondary constructor used in the PlugInAlgorithmNeruonSegmentation.
	 * The only difference is that instead of a <code>ModelImage</code> for 
	 * the targets, a <code>BitSet</code> is provided, which matches better
	 * with what occurs in that procedure.
	 *  
	 * @param costImage the cost to travel to a certain pixel, used to
	 * determine the shortest path
	 * @param visitedImage the targets for the Dijkstra algorithm. Once
	 * the path intersects ANY of the points in this <code>BitSet</code>, 
	 * the method ends.
	 * @param start the seed point from which to determine the path to 
	 * points on the visitedImage
	 * @param searchMode determines whether to use a cost function or use
	 * the standard Dijkstra algorithm (edge). 0 = cost function, and
	 * 1 = true Dijkstra's
	 */
	
	public PlugInAlgorithmDijkstraCode(ModelImage costImage, BitSet visitedImage, int start, int searchMode){
		
		super(null, costImage);

		extents = costImage.getExtents();
		width = extents[0];
		height = extents[1];
		length = width*height;
		
		//destImage = new ModelImage(ModelImage.BOOLEAN, extents, "Shortest Path");
		
		costs = new int[length];
		direction = new int[length];
		visited = new boolean[length];
		included = visitedImage;
		shortest = new BitSet(length);
		Arrays.fill(direction, -1);
		mode = searchMode;
		
		source = start;
		dijkstra = new PriorityQueue<Path>();
		
		try {
			costImage.exportData(0, length, costs);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Returns a concatenation of the shortest path with the original
	 * target image, which is useful for the neuron branching dialog.
	 * 
	 * @return the shortest path concatenated with the input image
	 */
	
	public BitSet getNewSkeleton(){
		
		BitSet temp = (BitSet) shortest.clone();
		temp.or(included);
		return temp;
	}
	
	public BitSet getShortestPath(){
		return shortest;
	}
	
	/**
	 * Dijkstra's algorithm for shortest path, modified to use a cost
	 * image/function instead of simply edge weights. 
	 */
	
	@Override
	public void runAlgorithm() {

		int x,y, nx, ny, ind;
		int edge;
		Path next;
		
		x = source%width;
		y = source/width;
		visited[source] = true;
		int node;
		
		//Start at initial seed point, start building paths
		for(int i=-1;i<=1;i++){
			ny = y+i;
			if(ny<0 || ny >= height) continue;
			for(int j=-1;j<=1;j++){
				nx = x+j;
				if(nx<0 || nx >= width) continue;
				if(i==0 && j==0) continue;
				ind = nx + ny*width;
				dijkstra.offer(new Path(source, ind, costs[ind]));
			}
		}

		while(true){
			
			//Check new point. Go to the next point if it has
			//already been visited, or stop if it reaches a
			//target point
			next = dijkstra.poll();
			if(visited[next.V2]) continue;
			x = next.V2 % width;
			y = next.V2 / width;
			visited[next.V2] = true;
			direction[next.V2] = next.V1;
			if(included.get(next.V2)) {
				node = next.V2;
				break; 
			}
			
			for(int i=-1;i<=1;i++){
				ny = y+i;
				if(ny<0 || ny >= height) continue;
				for(int j=-1;j<=1;j++){
					nx = x+j;
					if(nx<0 || nx >= width) continue;
					if(i==0 && j==0) continue;
					ind = nx + ny*width;
					if(visited[ind]) continue;
					if(mode == COST) dijkstra.offer(new Path(next.V2, ind, costs[ind] + next.W /*+ 5*/));
					//For true Dijkstra's with added path length bias, 
					//set mode to 1
					else{
						edge = Math.abs(costs[ind] - costs[next.V2]);
						dijkstra.offer(new Path(next.V2, ind, edge + next.W + 5));
					}
				}
			}
		}
		
		ind = node;
		
		shortest.set(source);
		while(ind != source){
			shortest.set(ind);
			ind = direction[ind];
		}
		
		setCompleted(true);

	}
	
	/**
	 * Structure to hold information about the current path,
	 * which is just the previou point, the current point, and
	 * the current cost of the given path.
	 * 
	 * Almost identical to the <code>Edge</code> class from
	 * PlugInAlgorithmGraphSegmentation (if for some reason
	 * I commited it)
	 * 
	 * @author wangvg
	 *
	 */
	
	private class Path implements Comparable<Path>{
		
		private int V1;
		
		private int V2;
		
		private int W;
		
		public Path(int u, int v, int weight){
			V1 = u;
			V2 = v;
			W = weight;
		}

		@Override
		public int compareTo(Path o) {
			int weight = this.W;
			int compare = o.W;
			if (weight > compare) return 1;
			else if (weight < compare) return -1;
			else return 0;
		}
	}

}
