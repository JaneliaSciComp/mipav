import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;


/**
 * Algorithm for Graph Segmentation
 * Source: Felzenszwalb and Huttenlocher, "Efficient Graph-Based Image Segmentation", 2004
 * @author wangvg
 *
 */

public class PlugInAlgorithmGraphSegmentation extends AlgorithmBase {

	private int[] srcBuffer;
	
	private int[] dstBuffer;
	
	private int width;
	
	private int height;
	
	private int depth;
	
	private int length;
	
	private Edge[] graph;
	
	//private Node[] nodes;
	
	private NodeContainer[] pixels;
	
	private int k;
	
	public PlugInAlgorithmGraphSegmentation( ModelImage destImage, ModelImage sourceImage, int k){
		super(destImage, sourceImage);
		this.k = k;
		int[] extents = srcImage.getExtents();
		width = extents[0];
		height = extents[1];
		if (extents.length==3) depth = extents[2];
		else depth = 1;
		length = width*height;
		
		//nodes = new Node[length];
		graph = new Edge[length*4 - 3*width - 3*height + 2];
		srcBuffer = new int[length*depth];
		dstBuffer = new int[length*depth];
		pixels = new NodeContainer[length];
		
		try {
			srcImage.exportData(0, length*depth, srcBuffer);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	@Override
	public void runAlgorithm() {

		fireProgressStateChanged(0, "Graph Segmentation", "Segmenting...");
		
		for(int z=0;z<depth;z++){
		
			populate(z);
			segment(z);
		
		}

		try {
			destImage.importData(0, dstBuffer, true);
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		dstBuffer = null;
		srcBuffer = null;
		graph = null;
		pixels = null;
		fireProgressStateChanged(100);
		setCompleted(true);
		
	}
	
	private int diff(int x, int y){

		int diff;
		
		//diff = (int)Math.sqrt(x*x+y*y);
		diff = Math.abs(x - y);
		return diff;
	}
	
	private void populate(int z){
		
		boolean up, down, right;
		int x, y, index, diff;
		int cnt;
		
		cnt = 0;
		for(int i=0;i<length;i++){
			x = i%width;
			y = i/width;
			up = y-1 >= 0;
			down = y+1 < height;
			right = x+1 < width;
			
			pixels[i] = new NodeContainer(i, srcBuffer[i + z*length]);
			
			if(right){
				index = x + 1 + y*width;

				diff = diff(srcBuffer[i + z*length], srcBuffer[index + z*length]);
				graph[cnt] = new Edge(i, index, diff);
				cnt++;
				if(up){
					index = x + 1 + (y-1)*width;
					diff = diff(srcBuffer[i + z*length], srcBuffer[index + z*length]);
					graph[cnt] = new Edge(i, index, diff);
					cnt++;
				}
				if(down){
					index = x + 1 + (y+1)*width;
					diff = diff(srcBuffer[i + z*length], srcBuffer[index + z*length]);
					graph[cnt] = new Edge(i, index, diff);
					cnt++;
				}
			}
			
			if(down){
				index = x + (y+1)*width;
				diff = diff(srcBuffer[i + z*length], srcBuffer[index + z*length]);
				graph[cnt] = new Edge(i, index, diff);
				cnt++;
			}
		}
		
		Arrays.sort(graph);
		
	}
	
	private void segment(int z){
		
		Edge edge;
		int V1, V2, W;
		NodeContainer nc, mc;
		Node n,m;
		float progress, ndiff, mdiff, mInt;
		int size = length*4 - 3*width - 3*height + 2;
		
		for(int i=0;i<size;i++){

			if(i%5000 == 0) {
				progress = 100*(float)i/(float)(size*depth) + 100*(float)z/(float)depth;
				fireProgressStateChanged((int)progress);
			}
			edge = graph[i];
			V1 = edge.V1;
			V2 = edge.V2;
			
			nc = pixels[V1];
			mc = pixels[V2];
			
			if(nc.equals(mc)) continue;
			
			W = edge.W;
			n = nc.linker;
			m = mc.linker;
			
			ndiff = (float)n.diff + (float)k/(float)(n.area);
			mdiff = (float)m.diff + (float)k/(float)(m.area);
			
			mInt = Math.min(ndiff, mdiff);
			
			if(W <= mInt){
				nc.union(mc, W);
			}
		}
		
		for(int i=0;i<length;i++){
			dstBuffer[i + z*length] = pixels[i].average();
		}
	}
	
	public void setK(int k){
		this.k = k;
	}

	
	private class NodeContainer{
		
		private Node linker;
		
		private NodeContainer(int i, int value){
			linker = new Node(i, value);
		}
		
		private void union(NodeContainer n, int W){
			if(this.linker.area > n.linker.area){
				this.linker.union(n.linker, W);
				update(this, n);
			}
			else {
				n.linker.union(this.linker, W);
				update(n, this);
			}
		}
		
		private int average(){
			return linker.sum/linker.area;
		}
		
		private void update(NodeContainer n, NodeContainer m){
			Iterator<Integer> itr = m.linker.tree.iterator();
			int index;
			while(itr.hasNext()){
				index = itr.next().intValue();
				pixels[index] = n;
			}
		}
		
	}
	
	private class Node{
		
		private int diff;
		
		private int sum;
		
		private int area;
		
		private HashSet<Integer> tree;
		
		private Node(){
			diff = 0;
			sum = 0;
			area = 0;
			tree = new HashSet<Integer>();
		}
		
		private Node(int i, int value){
			diff = 0;
			area = 1;
			tree = new HashSet<Integer>();
			tree.add(new Integer(i));
			sum = value;
		}
		
		/*private boolean equals(Node n){
			return this.tree.equals(n.tree);
		}*/
		private void union(Node n, int W){
			tree.addAll(n.tree);
			diff = W;
			sum += n.sum;
			area += n.area;
		}
		/*
		private int center(){
			Iterator<Integer> itr = tree.iterator();
			int center;
			int x = 0;
			int y = 0;
			int i;
			while(itr.hasNext()){
				i = itr.next().intValue();
				x += i%width;
				y += i/width;
			}
			x /= area;
			y /= area;
			
			center = x + y*width;
			return center;
		}*/

	}
	
	private class Edge implements Comparable<Edge>{
		
		private int V1;
		
		private int V2;
		
		private int W;
		
		public Edge(int u, int v, int weight){
			V1 = u;
			V2 = v;
			W = weight;
		}

		@Override
		public int compareTo(Edge o) {
			//Edge to = (Edge)o;
			int weight = this.W;
			int compare = o.W;
			if (weight > compare) return 1;
			else if (weight < compare) return -1;
			else return 0;
		}
	}

}
