package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.*;

import java.awt.Point;
import java.io.IOException;
import java.io.Serializable;
import java.util.*;

import Jama.Matrix;

/**Copyright (c) 2011, The University of Nottingham
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the University of Nottingham nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE UNIVERSITY OF NOTTINGHAM BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

This is a port of the original CellSet C# code into Java.
Porting done by William Gandler

*/

public class AlgorithmNetworkSnake extends AlgorithmBase {
	
	public enum AnchorPosition
    {
        North, South, East, West, None
    };
	
	public AlgorithmNetworkSnake(ModelImage srcImage) {
		super (null, srcImage);
	}
	
	public void runAlgorithm() {
		int nDims;
		int xDim;
		int yDim;
		int sliceSize;
		int zDim;
		int tDim;
		int z;
		int t;
		int i;
		int cf;
		double buffer[];
		
		if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        
        fireProgressStateChanged(0, srcImage.getImageName(),"Network Snakes ...");
        
        nDims = srcImage.getNDims();
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        if (nDims > 2) {
        	zDim = srcImage.getExtents()[2];
        }
        else {
        	zDim = 1;
        }
        if (nDims > 3) {
        	tDim = srcImage.getExtents()[3];
        }
        else {
        	tDim = 1;
        }
        if (srcImage.isColorImage()) {
        	cf = 4;
        }
        else {
        	cf = 1;
        }
        buffer = new double[cf * sliceSize];
        
        for (t = 0; t < tDim; t++) {
        	for (z = 0; z < zDim; z++) {
        		
        	   try {
     			   srcImage.exportData(cf*(t*zDim + z)*sliceSize, cf*sliceSize, buffer);
     		   }
     		   catch(IOException e) {
	               MipavUtil.displayError("IOException " + e + " on srcImage.exportData");
	        	   setCompleted(false);
	        	   return;
    		   }  
        	} // for (z = 0; z < zDim; z++)
        } // for (t = 0; t < tDim; t++)
        	
	} 
	
	public class NetworkSnake implements IMetadataProvider {
		private Metadata data;
		
		public NetworkSnake()
        {
            InitialiseData();
        }
		 public void InitialiseData()
	        {
	            data = new Metadata("snakes", "snake");
	        }
		 
		 public Metadata getData() {
			 return data;
		 }
		 
		 public void setData(Metadata data) {
			 this.data = data;
		 }
		 
		 private boolean isInitialised = false;
	        public boolean getIsInitialised()
	        {
	           return isInitialised;
	        }

	        private ArrayList<SnakeNode> nodes = new ArrayList<SnakeNode>();
	        public ArrayList<SnakeNode> getNodes() {
	        	return nodes;
	        }
	        public void setNodes(ArrayList<SnakeNode> nodes) {
	        	this.nodes = nodes;
	        }

	        private float alpha = 1.0f;
	        public float getAlpha() {
	        	return alpha;
	        }
	        public void setAlpha(float alpha) {
	        	this.alpha = alpha;
	        }
	        
	        private float beta = 1.0f;
	        public float getBeta() {
	        	return beta;
	        }
	        public void setBeta(float beta) {
	        	this.beta = beta;
	        }
	        

	        private float gamma = 1.0f;
	        private float getGamma() {
	        	return gamma;
	        }
	        public void setGamma(float gamma) {
	        	this.gamma = gamma;
	        }

	        private int spacing = 8;
	        
	        /*public void Initialise(SnakeInitialiser snakeInitialiser, int spacing)
	        {
	            // Set spacing
	            this.spacing = spacing;

	            // Create temporary sorted list to speed up processing
	            SortedList<Tuple<int, int>, SnakeNode> sortingList = new SortedList<Tuple<int, int>, SnakeNode>();

	            foreach (List<Point> nodePoints in snakeInitialiser.WallPositions)
	            {
	                List<Point> redistributedNodePoints = CreateAndRedistributePoints(nodePoints, spacing);

	                // Starting junction node
	                SnakeNode startNode = null;
	                Tuple<int,int> startKey = new Tuple<int, int>((int)redistributedNodePoints[0].X, (int)redistributedNodePoints[0].Y);
	                if (sortingList.IndexOfKey(startKey) >= 0)
	                    startNode = sortingList[startKey];
	                else
	                {
	                    startNode = new SnakeNode();
	                    startNode.X = (int)redistributedNodePoints[0].X;
	                    startNode.Y = (int)redistributedNodePoints[0].Y;
	                    sortingList.Add(startKey, startNode);
	                }

	                // Ending junction node
	                SnakeNode endNode = null;
	                Tuple<int,int> endKey = new Tuple<int, int>((int)redistributedNodePoints.Last().X, (int)redistributedNodePoints.Last().Y);
	                if (sortingList.IndexOfKey(endKey) >= 0)
	                    endNode = sortingList[endKey];
	                else
	                {
	                    endNode = new SnakeNode();
	                    endNode.X = (int)redistributedNodePoints.Last().X;
	                    endNode.Y = (int)redistributedNodePoints.Last().Y;
	                    sortingList.Add(endKey, endNode);
	                }

	                if (redistributedNodePoints.Count == 2)
	                {
	                    startNode.Neighbours.Add(endNode);
	                    endNode.Neighbours.Add(startNode);
	                }
	                else
	                {
	                    SnakeNode previousNode = startNode;
	                    for (int i = 1; i < redistributedNodePoints.Count - 1; i++)
	                    {
	                        int cX = (int)redistributedNodePoints[i].X, cY = (int)redistributedNodePoints[i].Y;
	                        Tuple<int,int> currentKey = new Tuple<int,int>(cX, cY);
	                        SnakeNode currentNode = new SnakeNode();
	                        currentNode.X = cX;
	                        currentNode.Y = cY;

	                        if (sortingList.ContainsKey(currentKey))
	                            continue;
	                        else
	                            sortingList.Add(currentKey, currentNode);

	                        // Link nodes
	                        previousNode.Neighbours.Add(currentNode);
	                        currentNode.Neighbours.Add(previousNode);
	                        previousNode = currentNode;
	                    }
	                    // Link final nodes
	                    previousNode.Neighbours.Add(endNode);
	                    endNode.Neighbours.Add(previousNode);
	                }
	            }

	            // Copy all nodes into the final snakes list - after this point ordering is not a concern
	            foreach (KeyValuePair<Tuple<int, int>, SnakeNode> kvp in sortingList)
	            {
	                this.nodes.Add(kvp.Value);
	            }

	            this.isInitialised = true;
	        }*/

	}
	
	 public class SnakeInitialiser implements Serializable
	    {
	        

	        public class Node implements Serializable
	        {
	            private int x, y;
	            public int getX() {
	            	return x;
	            }
	            public void setX(int x) {
	            	this.x = x;
	            }
	            public int getY() {
	            	return y;
	            }
	            public void setY(int y) {
	            	this.y = y;
	            }

	            private ArrayList<Node> neighbours;
	            public ArrayList<Node> getNeigbours() {
	            	return neighbours;
	            }
	            public void setNeighbours(ArrayList<Node> neighbours) {
	            	this.neighbours = neighbours;
	            }

	            private ArrayList<Node> junctionNeighbours;
	            public ArrayList<Node> getJunctionNeighbours() {
	            	return junctionNeighbours;
	            }
	            public void setJunctionNeighbours(ArrayList<Node> junctionNeighbours) {
	            	this.junctionNeighbours = junctionNeighbours;
	            }

	            private AnchorPosition anchor;
	            public AnchorPosition getAnchor() {
	            	return anchor;
	            }
	            public void setAnchor(AnchorPosition anchor) {
	            	this.anchor = anchor;
	            }
	           
	            public int getNeighbourCount()
	            {
	               return this.neighbours.size();
	            }

	            public Node()
	            {
	                this.neighbours = new ArrayList<Node>();
	                this.junctionNeighbours = new ArrayList<Node>();
	                this.anchor = AnchorPosition.None;
	            }

	            public Node clone()
	            {
	                Node n = new Node();
	                n.x = this.x;
	                n.y = this.y;
	                n.anchor = this.anchor;
	                return n;
	            }
	        } // public class Node implements Serializable
	        
	        // Interface SortedMap extends Map and maintains its keys in sorted order -
	        // either the element's natural order or an order specified by a Comparator.
	        // Class TreeMap implements SortedMap
	        private TreeMap<tuple2i, Node> nodeList = new TreeMap<tuple2i, Node>();
	        
	        public TreeMap<tuple2i, Node> getNodeList() {
	        	return nodeList;
	        }
	        
	        public void setNodeList(TreeMap<tuple2i, Node> nodeList) {
	            this.nodeList = nodeList;	
	        }
	        
	        private ArrayList<Node> externalNodes = null;
	        public ArrayList<Node> getExternalNodes() {
	        	return externalNodes;
	        }
	        public void setExternalNodes(ArrayList<Node> externalNodes) {
	        	this.externalNodes = externalNodes;
	        }
	        
	        private ArrayList<Node> internalNodes = null;
	        public ArrayList<Node> getInternalNodes() {
	        	return internalNodes;
	        }
	        public void setInternalNodes(ArrayList<Node> internalNodes) {
	        	this.internalNodes = internalNodes;
	        }
	        
	        private ArrayList<Node> linkingNodes = null;
	        public ArrayList<Node> getLinkingNodes() {
	        	return linkingNodes;
	        }
	        public void setLinkingNodes(ArrayList<Node> linkingNodes) {
	        	this.linkingNodes = linkingNodes;
	        }
	        
	        private ArrayList<ArrayList<Point>> wallPositions = new ArrayList<ArrayList<Point>>();
	        public ArrayList<ArrayList<Point>> getWallPositions() {
	            return wallPositions;	
	        }
	        public void setWallPositions(ArrayList<ArrayList<Point>> wallPositions) {
	        	this.wallPositions = wallPositions;
	        }
	        
	        private int recordRate = 8;
	        public int getRecordRate() {
	        	return recordRate;
	        }
	        public void setRecordRate(int recordRate) {
	            this.recordRate = recordRate;	
	        }
	        
	        private double mergeThreshold = 3.0;
	        public double getMergeThreshold() {
	        	return mergeThreshold;
	        }
	        public void setMergeThreshold(double mergeThreshold) {
	        	this.mergeThreshold = mergeThreshold;
	        }
	        
	        public boolean exists(int x, int y)
	        {
	            return (this.nodeList.containsKey(new tuple2i(x, y)));
	        }
	        
	        public SnakeInitialiser()
	        {

	        }

	        public SnakeInitialiser clone()
	        {
	            // Perform a deep copy of the SnakeInitialiser
	            SnakeInitialiser newSnakeInitialiser = new SnakeInitialiser();
	            newSnakeInitialiser.recordRate = this.recordRate;

	            // Create all the basic nodes, with no neighbours
	            Set set = this.nodeList.entrySet();
	            Iterator iterator = set.iterator();
	            while (iterator.hasNext()) {
	            	Map.Entry mentry = (Map.Entry)iterator.next();
	            	newSnakeInitialiser.nodeList.put(new tuple2i(((tuple2i)mentry.getKey()).a, ((tuple2i)mentry.getKey()).b), ((Node)mentry.getValue()).clone());
	            }

	            // Create all neighbour links
	            set = this.nodeList.entrySet();
	            iterator = set.iterator();
	            while (iterator.hasNext()) {
	            	Map.Entry mentry = (Map.Entry)iterator.next();
	                tuple2i currentKey = new tuple2i(((tuple2i)mentry.getKey()).a, ((tuple2i)mentry.getKey()).b);
	                Node sourceNode = (Node)mentry.getValue();
	                Node destinationNode = newSnakeInitialiser.nodeList.get(currentKey);
	                for (Node neighbouringNode : sourceNode.neighbours)
	                {
	                    tuple2i neighbouringKey = new tuple2i(neighbouringNode.x, neighbouringNode.y);
	                    destinationNode.neighbours.add(newSnakeInitialiser.nodeList.get(neighbouringKey));
	                }

	                for (Node neighbouringJunctionNode : sourceNode.junctionNeighbours)
	                {
	                    tuple2i neighbouringKey = new tuple2i(neighbouringJunctionNode.x, neighbouringJunctionNode.y);
	                    destinationNode.junctionNeighbours.add(newSnakeInitialiser.nodeList.get(neighbouringKey));
	                }
	            }
	            return newSnakeInitialiser;
	        }
	    }
	 
	 public class tuple2i {

		    public final int a;
		    public final int b;

		    public tuple2i(int a, int b) {
		        this.a = a;
		        this.b = b;
		    }

		}

	
	 public class SnakeNode implements Comparable
	    {
	        /// The X and Y Positions of the snake node
	        protected int x, y;
	        public int getX() {
	        	return x;
	        }
	        public void setX(int x) {
	        	this.x = x;
	        }
            public int getY() {
            	return y;
            }
	        
            public void setY(int y) {
            	this.y = y;
            }
	       
          /// A list of all neigbouring snake nodes
	        private ArrayList<SnakeNode> neighbours;
	        public ArrayList<SnakeNode> getNeighbours() {
	        	return neighbours;
	        }
	        public void setNeighbours(ArrayList<SnakeNode> neighbours) {
	        	this.neighbours = neighbours;
	        }
	        
	      /// A list of boolean values associated with each neighbour. These are flagged on and off during iteration to simplify processing
	        private ArrayList<Boolean> neighbourLink;
	        public ArrayList<Boolean> getNeighbourLink() {
	        	return neighbourLink;
	        }
	        public void setNeighbourLink(ArrayList<Boolean> neighbourLink) {
	        	this.neighbourLink = neighbourLink;
	        }
	       
	        /// The snakenode constructor
	        public SnakeNode()
	        {
	            this.neighbours = new ArrayList<SnakeNode>();
	        }
	        
	        /// <summary>
	        /// Compares a snakenode to another object. Will fail if the object is not a snakenode.
	        /// </summary>
	        /// <param name="obj">A snakenode that is to be compared with the current snakenode</param>
	        /// <returns>An integer either +ve, -ve or zero after comparing the X and Y positions of both snake nodes</returns>
	        public int compareTo(Object obj)
	        {
	            if (obj instanceof SnakeNode)
	            {
	                SnakeNode compare = (SnakeNode)obj;
	                if (this.x > compare.x) {
	                	return 1;
	                }
	                else if (this.x < compare.x) {
	                	return -1;
	                }
	                else if (this.y > compare.y) {
	                	return 1;
	                }
	                else if (this.y < compare.y) {
	                	return -1;
	                }
	                else {
	                	return 0;
	                }
	            }
	            MipavUtil.displayError("Object is not a Snakenode");
	            return Integer.MAX_VALUE;
	        }
	        
	        /// Obtains the average distance between a snake node and its neighbours for an entire line of interlinked snake nodes
	        /// <returns>The average distance between snake nodes in a chain, ending at junction nodes</returns>
	        public float findAverageDistance()
	        {
	        	int diffX;
	        	int diffY;
	            if (this.neighbours.size() != 2)
	                return 0.0f;

	            float totalDistance = 0.0f;
	            int totalSegments = 0;


	            // Travel Left
	            SnakeNode currentNode = this;
	            SnakeNode leftNode = this.neighbours.get(0);
	            while (leftNode.neighbours.size() == 2)
	            {
	                // Add distance and total
	            	diffX = currentNode.x - leftNode.x;
	            	diffY = currentNode.y - leftNode.y;
	                totalDistance += (float)Math.sqrt(diffX*diffX + diffY*diffY);
	                totalSegments++;

	                // Next leftNode and currentNode
	                if (leftNode.neighbours.get(0) == currentNode)
	                {
	                    currentNode = leftNode;
	                    leftNode = leftNode.neighbours.get(1);
	                }
	                else
	                {
	                    currentNode = leftNode;
	                    leftNode = leftNode.neighbours.get(0);
	                }
	            }

	            // Last left segment
	            diffX = currentNode.x - leftNode.x;
	            diffY = currentNode.y - leftNode.y;
	            totalDistance += (float)Math.sqrt(diffX*diffX + diffY*diffY);
	            totalSegments++;

	            currentNode = this;
	            SnakeNode rightNode = this.neighbours.get(1);
	            while (rightNode.neighbours.size() == 2)
	            {
	                // Add distance and total
	            	diffX = currentNode.x - rightNode.x;
	            	diffY = currentNode.y - rightNode.y;
	            	totalDistance += (float)Math.sqrt(diffX*diffX + diffY*diffY);
	                totalSegments++;

	                // Next rightNode and currentNode
	                if (rightNode.neighbours.get(0) == currentNode)
	                {
	                    currentNode = rightNode;
	                    rightNode = rightNode.neighbours.get(1);
	                }
	                else
	                {
	                    currentNode = rightNode;
	                    rightNode = rightNode.neighbours.get(0);
	                }
	            }

	            // Last right segment
	            diffX = currentNode.x - rightNode.x;
	            diffY = currentNode.y - rightNode.y;
	            totalDistance += (float)Math.sqrt(diffX*diffX + diffY*diffY);
	            totalSegments++;

	            return totalDistance / totalSegments;
	        }
	        
	      /// Iterates the snake node
	        /// </summary>
	        /// <param name="alpha">The curvature term</param>
	        /// <param name="beta">The continuity term</param>
	        /// <param name="gamma">The image term</param>
	        /// <param name="d">The average distance between all snakes on this wall</param>
	        /// <param name="imageBuffer">The image data to be used with the image energy term</param>
	        /// <param name="bufferWidth">The width of the image data buffer</param>
	        /// <param name="bufferHeight">The height of the image data buffer</param>
	        /// <param name="m">The radius of the local window in which to iterate</param>
	        /// <returns>Returns true if the snake node has moved, false if it is already in the optimal location</returns>
	        public boolean iterate(float alpha, float beta, float gamma, float d, byte[] imageBuffer, int bufferWidth, int bufferHeight, int m)
	        {
	        	float diffX;
	        	float diffY;
	            // Record starting position
	            Point startingPosition = new Point(this.x, this.y);

	            // Iterates the SnakeNode once, moving it to a nearby position that minimises internal and external energy
	            if (this.neighbours.size() == 1)
	            {
	                int xMin = 0, xMax = 0, yMin = 0, yMax = 0;

	                // Anchor nodes
	                if (this.x == 0)
	                {
	                    yMin = Math.max(0, this.y - m);
	                    yMax = Math.min(this.y + m, bufferHeight - 1);
	                }
	                else if (this.x == bufferWidth - 1)
	                {
	                    xMin = bufferWidth - 1;
	                    xMax = xMin;
	                    yMin = Math.max(0, this.y - m);
	                    yMax = Math.min(this.y + m, bufferHeight - 1);
	                }
	                else if (this.y == 0)
	                {
	                    xMin = Math.max(0, this.x - m);
	                    xMax = Math.min(this.x + m, bufferWidth - 1);
	                }
	                else if (this.y == bufferHeight - 1)
	                {
	                    yMin = bufferHeight - 1;
	                    yMax = yMin;
	                    xMin = Math.max(0, this.x - m);
	                    xMax = Math.min(this.x + m, bufferWidth - 1);
	                }
	                else
	                {
	                    MipavUtil.displayError("Anchor junction found outside of image border");
	                    return false;
	                }

	                // No Econt

	                // Ecurve
	                float[][] Ecurve = new float[xMax - xMin + 1] [yMax - yMin + 1];
	                float EcurveMax = Float.MIN_VALUE;
	                Point p1 = new Point (this.neighbours.get(0).x, this.neighbours.get(0).y);
	                Point p2 = new Point (0, 0);

	                boolean useEcurve = true;

	                if (this.neighbours.get(0).neighbours.size() == 2)
	                {
	                    // Find correct x2 and y2 neighbour
	                    if (this.neighbours.get(0).neighbours.get(0) == this)
	                    {
	                        p2 = new Point( this.neighbours.get(0).neighbours.get(1).x, this.neighbours.get(0).neighbours.get(1).y);
	                    }
	                    else
	                    {
	                        p2 = new Point( this.neighbours.get(0).neighbours.get(0).x, this.neighbours.get(0).neighbours.get(0).y);
	                    }
	                }
	                else
	                {
	                    p2 = new Point( this.x, this.y);
	                    useEcurve = false;
	                }
	                
	                int ypos = 0;
	                for (int y = yMin; y <= yMax; y++)
	                {
	                    int xpos = 0;
	                    for (int x = xMin; x <= xMax; x++)
	                    {
	                        Point p0 = new Point(x, y);
	                        diffX = ((p0.x - p1.x) - (p1.x - p2.x));
	                        diffY = ((p0.y - p1.y) - (p1.y - p2.y));
	                        Ecurve[xpos][ ypos] = diffX*diffX + diffY*diffY;
	                        
	                        if (Ecurve[xpos][ ypos] > EcurveMax)
	                            EcurveMax = Ecurve[xpos][ ypos];
	                        xpos++;
	                    }
	                    ypos++;
	                }

	                // Normalise Ecurve
	                if (useEcurve)
	                {
	                    for (int y = 0; y < Ecurve[0].length; y++)
	                        for (int x = 0; x < Ecurve.length; x++)
	                            Ecurve[x][ y] /= EcurveMax;
	                }
	                else
	                {
	                    // Immediately next to a junction, meaning curvature is impossible to calculate
	                    for (int y = 0; y < Ecurve[0].length; y++)
	                        for (int x = 0; x < Ecurve.length; x++)
	                            Ecurve[x][ y] = 0.0f;
	                }

	                // Eimage
	                float[][] Eimage = new float[xMax - xMin + 1][ yMax - yMin + 1];
	                float EimageMin = Float.MAX_VALUE;
	                float EimageMax = Float.MIN_VALUE;

	                ypos = 0;
	                for (int y = yMin; y <= yMax; y++)
	                {
	                    int xpos = 0;
	                    for (int x = xMin; x <= xMax; x++)
	                    {
	                        Eimage[xpos][ ypos] = 255 - imageBuffer[y * bufferWidth + x];

	                        if (Eimage[xpos][ ypos] > EimageMax)
	                            EimageMax = Eimage[xpos][ ypos];

	                        if (Eimage[xpos][ ypos] < EimageMin)
	                            EimageMin = Eimage[xpos][ ypos];
	                        xpos++;
	                    }
	                    ypos++;
	                }

	                // Normalise Eimage
	                for (int y = 0; y < Eimage[0].length; y++)
	                    for (int x = 0; x < Eimage.length; x++)
	                        Eimage[x][ y] = (Eimage[x][ y] - EimageMin) / Math.max(1, EimageMax - EimageMin);

	                // Choose final position for snake point
	                float EMin = Float.MAX_VALUE;
	                int EminX = 0, EminY = 0;
	                ypos = 0;
	                for (int y = yMin; y <= yMax; y++)
	                {
	                    int xpos = 0;
	                    for (int x = xMin; x <= xMax; x++)
	                    {
	                        float E = beta * Ecurve[xpos][ ypos] + gamma * Eimage[xpos][ ypos];

	                        if (E < EMin)
	                        {
	                            EMin = E;
	                            EminX = x;
	                            EminY = y;
	                        }
	                        xpos++;
	                    }
	                    ypos++;
	                }

	                this.x = EminX;
	                this.y = EminY;
	            }
	            else if (this.neighbours.size() == 2)
	            {
	                // Calculate bounds
	                int xMin = Math.max(0, this.x - m);
	                int xMax = Math.min(this.x + m, bufferWidth - 1);
	                int yMin = Math.max(0, this.y - m);
	                int yMax = Math.min(this.y + m, bufferHeight - 1);

	                // Econt
	                float[][] Econt = new float[xMax - xMin + 1][ yMax - yMin + 1];
	                float EcontMax = Float.MIN_VALUE;
	                int ypos = 0;
	                int x2 = this.neighbours.get(0).x;
	                int y2 = this.neighbours.get(0).y;
	                for (int y = yMin; y <= yMax; y++)
	                {
	                    int xpos = 0;
	                    for (int x = xMin; x <= xMax; x++)
	                    {
	                        Econt[xpos][ ypos] = (float)Math.pow(x - x2, 2.0) + (float)Math.pow(y - y2, 2.0);
	                        Econt[xpos][ ypos] = (float)Math.sqrt(Econt[xpos][ ypos]);
	                        Econt[xpos][ ypos] = (float)Math.pow(d - Econt[xpos][ ypos],2.0);
	                        if (Econt[xpos][ ypos] > EcontMax)
	                            EcontMax = Econt[xpos][ ypos];
	                        xpos++;
	                    }
	                    ypos++;
	                }

	                // Normalise Econt
	                for (int y = 0; y < Econt[0].length; y++)
	                    for (int x = 0; x < Econt.length; x++)
	                        Econt[x][ y] /= EcontMax;

	                // Ecurve
	                float[][] Ecurve = new float[xMax - xMin + 1][ yMax - yMin + 1];
	                float EcurveMax = Float.MIN_VALUE;
	                int x0 = this.neighbours.get(0).x;
	                int y0 = this.neighbours.get(0).y;
	                x2 = this.neighbours.get(1).x;
	                y2 = this.neighbours.get(1).y;
	                ypos = 0;
	                for (int y = yMin; y <= yMax; y++)
	                {
	                    int xpos = 0;
	                    for (int x = xMin; x <= xMax; x++)
	                    {
	                        Ecurve[xpos][ ypos] = (float)Math.pow((x0 - 2 * x + x2), 2.0) + (float)Math.pow((y0 - 2 * y + y2), 2.0);

	                        if (Ecurve[xpos][ ypos] > EcurveMax)
	                            EcurveMax = Ecurve[xpos][ ypos];
	                        xpos++;
	                    }
	                    ypos++;
	                }

	                // Normalise Ecurve
	                for (int y = 0; y < Ecurve[0].length; y++)
	                    for (int x = 0; x < Ecurve.length; x++)
	                        Ecurve[x][ y] /= EcurveMax;


	                // Eimage
	                float[][] Eimage = new float[xMax - xMin + 1][ yMax - yMin + 1];
	                float EimageMin = Float.MAX_VALUE;
	                float EimageMax = Float.MIN_VALUE;

	                ypos = 0;
	                for (int y = yMin; y <= yMax; y++)
	                {
	                    int xpos = 0;
	                    for (int x = xMin; x <= xMax; x++)
	                    {
	                        Eimage[xpos][ ypos] = 255 - imageBuffer[y * bufferWidth + x];

	                        if (Eimage[xpos][ ypos] > EimageMax)
	                            EimageMax = Eimage[xpos][ ypos];

	                        if (Eimage[xpos][ ypos] < EimageMin)
	                            EimageMin = Eimage[xpos][ ypos];
	                        xpos++;
	                    }
	                    ypos++;
	                }

	                // Normalise Eimage
	                for (int y = 0; y < Eimage[0].length; y++)
	                    for (int x = 0; x < Eimage.length; x++)
	                        Eimage[x][ y] = (Eimage[x][ y] - EimageMin) / Math.max(1, EimageMax - EimageMin);

	                // Choose final position for snake point
	                float EMin = Float.MAX_VALUE;
	                int EminX = 0, EminY = 0;
	                ypos = 0;
	                for (int y = yMin; y <= yMax; y++)
	                {
	                    int xpos = 0;
	                    for (int x = xMin; x <= xMax; x++)
	                    {
	                        float E = alpha * Econt[xpos][ ypos] + beta * Ecurve[xpos][ ypos] + gamma * Eimage[xpos][ ypos];

	                        if (E < EMin)
	                        {
	                            boolean neighbourConflict = false;
	                            for (SnakeNode neighour : this.neighbours)
	                            {
	                                if (x == neighour.x && y == neighour.y)
	                                {
	                                    neighbourConflict = true;
	                                    break;
	                                }
	                            }

	                            if (neighbourConflict)
	                                continue;

	                            EMin = E;
	                            EminX = x;
	                            EminY = y;
	                        }
	                        xpos++;
	                    }
	                    ypos++;
	                }

	                if (EMin != Float.MAX_VALUE)
	                {
	                    this.x = EminX;
	                    this.y = EminY;
	                }
	            }
	            else
	            {
	                // Junction nodes

	                // Find nearest junctions to calculate Rmax
	                ArrayList<SnakeNode> junctionNeighbours = this.findJunctionNeighbours();
	                double minimumJunctionDistance = Double.MAX_VALUE;

	                for (SnakeNode junctionNeighbour : junctionNeighbours)
	                {
	                    // Ignore boundary nodes
	                    if (junctionNeighbour.neighbours.size() != 1)
	                    {
	                        double junctionDistance = Math.sqrt(Math.pow( this.x - junctionNeighbour.x, 2.0) + Math.pow(this.y - junctionNeighbour.y, 2.0));
	                        if (junctionDistance < minimumJunctionDistance)
	                            minimumJunctionDistance = junctionDistance;
	                    }
	                }

	                int Rmax = (int)(minimumJunctionDistance / 2.0);
	                int Rmin = 4;
	                int offsetX[] = new int[1];
	                int offsetY[] = new int[1];
	                byte[][] thresholdBuffer = findThresholdedRegion(this.x, this.y, imageBuffer, bufferWidth, bufferHeight, Rmax + m, true, offsetX, offsetY);

	                // Calculate bounds
	                int xMin = Math.max(0, this.x - m);
	                int xMax = Math.min(this.x + m, bufferWidth - 1);
	                int yMin = Math.max(0, this.y - m);
	                int yMax = Math.min(this.y + m, bufferHeight - 1);

	                // No Econt

	                // Ecurve
	                float[][] Ecurve = new float[xMax - xMin + 1][ yMax - yMin + 1];
	                float EcurveMax = Float.MIN_VALUE;

	                ArrayList<Point[]> neighbouringPoints = new ArrayList<Point[]>();

	                int neighbourIndex = 0;
	                for (SnakeNode neighbour : this.neighbours)
	                {
	                    neighbouringPoints.add(new Point[2]);
	                    neighbouringPoints.get(neighbourIndex)[0] = new Point(neighbour.x, neighbour.y);
	                    if (neighbour.neighbours.size() == 2)
	                    {
	                        // Find correct next neighbour
	                        if (neighbour.neighbours.get(0) == this)
	                        {
	                            neighbouringPoints.get(neighbourIndex)[1] = new Point(neighbour.neighbours.get(1).x, neighbour.neighbours.get(1).y);
	                        }
	                        else
	                        {
	                            neighbouringPoints.get(neighbourIndex)[1] = new Point(neighbour.neighbours.get(0).x, neighbour.neighbours.get(0).y);
	                        }
	                    }
	                    else
	                    {
	                        // Current approach when n-2 doesn't exist is to not use that direction in the weight
	                        neighbouringPoints.remove(neighbouringPoints.size() - 1);
	                        continue;
	                    }
	                    neighbourIndex++;
	                }

	                int ypos = 0;
	                for (int y = yMin; y <= yMax; y++)
	                {
	                    int xpos = 0;
	                    for (int x = xMin; x <= xMax; x++)
	                    {
	                        Point p0 = new Point(x, y);
	                        Ecurve[xpos][ ypos] = 0;
	                        for (Point[] currentPoints : neighbouringPoints)
	                        {
	                            Point p1 = currentPoints[0];
	                            Point p2 = currentPoints[1];
	                            diffX = ((p0.x - p1.x) - (p1.x - p2.x));
	                            diffY = ((p0.y - p1.y) - (p1.y - p2.y));
	                            Ecurve[xpos][ ypos] += (diffX*diffX + diffY*diffY);
	                        }
	                        if (Ecurve[xpos][ ypos] > EcurveMax)
	                            EcurveMax = Ecurve[xpos][ ypos];
	                        xpos++;
	                    }
	                    ypos++;
	                }

	                // Normalise Ecurve
	                for (int y = 0; y < Ecurve[0].length; y++)
	                    for (int x = 0; x < Ecurve.length; x++)
	                        Ecurve[x][ y] /= EcurveMax;

	                // Eimage
	                float[][] Eimage = new float[xMax - xMin + 1][ yMax - yMin + 1];
	                float EimageMin = Float.MAX_VALUE;
	                float EimageMax = Float.MIN_VALUE;

	                ypos = 0;
	                for (int y = yMin; y <= yMax; y++)
	                {
	                    int xpos = 0;
	                    for (int x = xMin; x <= xMax; x++)
	                    {
	                        // Calculate Eimage based on concentric circles
	                        int xCentre = xMax - xMin;

	                        Eimage[xpos][ ypos] = findJunctionAppearanceWeight(x - offsetX[0], y - offsetY[0], thresholdBuffer, Rmax, Rmin);

	                        if (Eimage[xpos][ ypos] > EimageMax)
	                            EimageMax = Eimage[xpos][ ypos];

	                        if (Eimage[xpos][ ypos] < EimageMin)
	                            EimageMin = Eimage[xpos][ ypos];
	                        xpos++;
	                    }
	                    ypos++;
	                }

	                // Normalise Eimage
	                for (int y = 0; y < Eimage[0].length; y++)
	                    for (int x = 0; x < Eimage.length; x++)
	                        Eimage[x][ y] = (Eimage[x][ y] - EimageMin) / Math.max(1, EimageMax - EimageMin);

	                // Choose final position for snake point
	                float EMin = Float.MAX_VALUE;
	                int EminX = 0, EminY = 0;
	                ypos = 0;
	                for (int y = yMin; y <= yMax; y++)
	                {
	                    int xpos = 0;
	                    for (int x = xMin; x <= xMax; x++)
	                    {
	                        // Energy formula
	                        float E = beta * Ecurve[xpos][ ypos] + gamma * Eimage[xpos][ ypos];

	                        if (E < EMin)
	                        {
	                            boolean neighbourConflict = false;
	                            for (SnakeNode neighour : this.neighbours)
	                            {
	                                if (x == neighour.x && y == neighour.y)
	                                {
	                                    neighbourConflict = true;
	                                    break;
	                                }
	                            }

	                            if (neighbourConflict)
	                                continue;

	                            EMin = E;
	                            EminX = x;
	                            EminY = y;
	                        }
	                        xpos++;
	                    }
	                    ypos++;
	                }

	                if (EMin != Float.MAX_VALUE)
	                {
	                    this.x = EminX;
	                    this.y = EminY;
	                }
	            }

	            Point finalPosition = new Point(this.x, this.y);
	            return startingPosition != finalPosition;
	        } 
	        
	      /// Finds the appearance weighting of a junction node using the Sethuraman junction weight
	        /// </summary>
	        /// <param name="x">The x position of the node</param>
	        /// <param name="y">The y position of the node</param>
	        /// <param name="thresholdBuffer">A binary threshold buffer of that region</param>
	        /// <param name="Rmax">The maximum radius around the node to use for this weighting</param>
	        /// <param name="Rmin">The minimum radius around the node to use for this weighting</param>
	        /// <returns>A integer weighting value</returns>
	        protected int findJunctionAppearanceWeight(int x, int y, byte[][] thresholdBuffer, int Rmax, int Rmin)
	        {
	            int weightingFactor = thresholdBuffer[x][y] > 0 ? 1 : 0;
	            boolean onWall = false;
	            //double root2 = Math.sqrt(2.0);
	            final int minWallWidth = 4;
	            int bufferWidth = thresholdBuffer.length;
	            int bufferHeight = thresholdBuffer[0].length;
	            double angleToRadians = Math.PI/180.0;
	          
	            for (int currentRadius = Rmax; currentRadius >= Rmin; currentRadius--)
	            {
	                int intersectionCount = 0;
	                int rotationStep = 4;
	                int wallWidth = 0;
	                //Vector V = new Vector(0, -currentRadius);
	                //Matrix rotationMatrix = Matrix.Identity;
	                //rotationMatrix.setRotate(rotationStep);

	                for (int rotation = 0; rotation <= 360; rotation += rotationStep)
	                {
	                	double radians = (rotation + rotationStep) * angleToRadians;
	                	double cosTheta = Math.cos(radians);
	                	double sinTheta = Math.sin(radians);
	                	// xp = x*cosTheta - y*sinTheta;
	                	// yp = x*sinTheta + y*cosTheta;
	                	double xp = currentRadius * sinTheta;
	                	double yp = -currentRadius * cosTheta;
	                    //Vector position = rotationMatrix.Transform(V);
	                    //int bufferPositionX = (int)position.X + x;
	                    //int bufferPositionY = (int)position.Y + y;
	                	int bufferPositionX = (int)xp + x;
	                	int bufferPositionY = (int)yp + y;

	                    // Inside the threshold buffer
	                    bufferPositionX = ExtentionMath.clamp(bufferPositionX, 0, bufferWidth - 1);
	                    bufferPositionY = ExtentionMath.clamp(bufferPositionY, 0, bufferHeight - 1);

	                    if (thresholdBuffer[bufferPositionX][ bufferPositionY] > 0)
	                    {
	                        if (wallWidth >= minWallWidth && !onWall)
	                        {
	                            onWall = true;
	                            intersectionCount++;
	                        }
	                        else
	                        {
	                            wallWidth++;
	                        }
	                    }
	                    else if (thresholdBuffer[bufferPositionX][ bufferPositionY] == 0)
	                    {
	                        onWall = false;
	                        wallWidth = 0;
	                    }
	                    //rotationMatrix.setRotate(rotationStep);
	                }

	                if (intersectionCount > 2)
	                    weightingFactor++;
	            }
	            return (int)Math.pow(weightingFactor,3.0);
	        }
	        
	        /// Creates a gaussian operator
	        /// </summary>
	        /// <param name="radius">The radius of this operator</param>
	        /// <param name="sigma1">The sigma value for this operator</param>
	        /// <returns>A two-dimensional array containing this operator</returns>
	        private double[][] createGOperator(int radius, double sigma1)
	        {
	            int dimension = radius * 2 + 1;
	            double[][] matrix = new double[dimension][ dimension];

	            double TwoSigma2 = 2 * Math.pow(sigma1, 2.0);

	            for (int y = -radius; y <= radius; y++)
	            {
	                for (int x = -radius; x <= radius; x++)
	                {
	                    double G1 = 1 / (TwoSigma2 * Math.PI);
	                    G1 *= Math.pow(Math.E, -(Math.pow(x, 2) + Math.pow(y, 2)) / TwoSigma2);
	                    matrix[x + radius][ y + radius] = G1;
	                }
	            }
	            return matrix;
	        }
	        
	        /// Creates a difference of gaussian operator
	        /// </summary>
	        /// <param name="radius">The radius of this operator</param>
	        /// <param name="sigma1">The sigma value for the first (positive gaussian)</param>
	        /// <param name="K">The ratio of the first sigma value to the second</param>
	        /// <param name="scale1">The scale of the first gaussian</param>
	        /// <param name="scale2">The scale of the second gaussian</param>
	        /// <returns>A two-dimensional array containing this operator</returns>
	        private double[][] createDoGOperator(int radius, double sigma1, double K, double scale1, double scale2)
	        {
	            int dimension = radius * 2 + 1;
	            double[][] matrix = new double[dimension][ dimension];

	            double TwoSigma2 = 2 * Math.pow(sigma1, 2.0);
	            double TwoK2Sigma2 = 2 * Math.pow(K, 2.0) * Math.pow(sigma1, 2.0);

	            for (int y = -radius; y <= radius; y++)
	            {
	                for (int x = -radius; x <= radius; x++)
	                {
	                    double G1 = 1 / (TwoSigma2 * Math.PI);
	                    G1 *= Math.pow(Math.E, -(Math.pow(x, 2) + Math.pow(y, 2)) / TwoSigma2);

	                    double G2 = 1 / (TwoK2Sigma2 * Math.PI);
	                    G2 *= Math.pow(Math.E, -(Math.pow(x, 2) + Math.pow(y, 2)) / TwoK2Sigma2);

	                    matrix[x + radius][ y + radius] = scale1 * G1 - scale2 * G2;
	                }
	            }
	            return matrix;
	        }
	        
	        /// Obtains the thresholded region in an image buffer, for use with the junction weighting algorithm
	        /// </summary>
	        /// <param name="x">The x position of the centre of this region</param>
	        /// <param name="y">The y position of the centre of this region</param>
	        /// <param name="imageBuffer">The image buffer used as a source for thresholding</param>
	        /// <param name="bufferWidth">The width of the image buffer</param>
	        /// <param name="bufferHeight">The height of the image buffer</param>
	        /// <param name="radius">The radius of the region to threshold</param>
	        /// <param name="thresholding">A boolean stating whether to apply a threshold to this smaller region in the image buffer</param>
	        /// <param name="offsetX">The X offset of the resulting region relative to the original source</param>
	        /// <param name="offsetY">The Y offset of the resulting region relative to the original source</param>
	        /// <returns>A byte array containing the thresholded sub-region of the source image</returns>
	        protected byte[][] findThresholdedRegion(int x, int y, byte[] imageBuffer, int bufferWidth, int bufferHeight, int radius, boolean thresholding,
	        		int offsetX[], int offsetY[])
	        {
	            int xMin = Math.max(0, x - radius);
	            int xMax = Math.min(bufferWidth - 1, x + radius);
	            int yMin = Math.max(0, y - radius);
	            int yMax = Math.min(bufferHeight - 1, y + radius);

	            byte[][] regionBuffer = new byte[xMax - xMin + 1][ yMax - yMin + 1];
	            int totalIntensity = 0;

	            offsetX[0] = xMin;
	            offsetY[0] = yMin;

	            int ypos = 0;
	            for (int bY = yMin; bY <= yMax; bY++)
	            {
	                int xpos = 0;
	                for (int bX = xMin; bX <= xMax; bX++)
	                {
	                    regionBuffer[xpos][ ypos] = imageBuffer[bY * bufferWidth + bX];
	                    totalIntensity += regionBuffer[xpos][ ypos];
	                    xpos++;
	                }
	                ypos++;
	            }

	            // Threshold using mean of intensity level
	            if (thresholding)
	            {
	                int threshold = totalIntensity / (regionBuffer.length * regionBuffer[0].length);
	                for (int xpos = 0; xpos < regionBuffer.length; xpos++)
	                {
	                    for (ypos = 0; ypos < regionBuffer[0].length; ypos++)
	                    {
	                        regionBuffer[xpos][ ypos] = (byte)(regionBuffer[xpos][ ypos] >= threshold ? 255 : 0);
	                    }
	                }
	            }
	            return regionBuffer;
	        }
	        
	      /// Given a snake node, returns a list of all snake nodes up to and including the surrounding junction nodes
	        /// </summary>
	        /// <returns>A list of snake nodes along the wall</returns>
	        public ArrayList<SnakeNode> findWall()
	        {
	            if (this.neighbours.size() != 2)
	                return null;
	            else
	            {
	                ArrayList<SnakeNode> neighbours0 = new ArrayList<SnakeNode>();
	                ArrayList<SnakeNode> neighbours1 = new ArrayList<SnakeNode>();

	                // 0
	                SnakeNode neighbour = this.neighbours.get(0);
	                SnakeNode previousNode = this;
	                SnakeNode nextNode = neighbour;

	                while (nextNode.neighbours.size() == 2)
	                {
	                    neighbours0.add(nextNode);
	                    if (nextNode.neighbours.get(0) == previousNode)
	                    {
	                        previousNode = nextNode;
	                        nextNode = nextNode.neighbours.get(1);
	                    }
	                    else
	                    {
	                        previousNode = nextNode;
	                        nextNode = nextNode.neighbours.get(0);
	                    }
	                }
	                neighbours0.add(nextNode);

	                // 1
	                neighbour = this.neighbours.get(1);
	                previousNode = this;
	                nextNode = neighbour;

	                while (nextNode.neighbours.size() == 2)
	                {
	                    neighbours1.add(nextNode);
	                    if (nextNode.neighbours.get(0) == previousNode)
	                    {
	                        previousNode = nextNode;
	                        nextNode = nextNode.neighbours.get(1);
	                    }
	                    else
	                    {
	                        previousNode = nextNode;
	                        nextNode = nextNode.neighbours.get(0);
	                    }
	                }
	                neighbours1.add(nextNode);

	                // Combine lists
	                ArrayList<SnakeNode> wallNodes = new ArrayList<SnakeNode>();

	                for (int i = neighbours0.size() - 1; i >= 0; i--)
	                {
	                    wallNodes.add(neighbours0.get(i));
	                }
	                wallNodes.add(this);
	                for (int i = 0; i < neighbours1.size(); i++)
	                {
	                    wallNodes.add(neighbours1.get(i));
	                }

	                return wallNodes;
	            }
	        }
	        
	        /// Finds a list of the closest neighbouring junctions to a snakenode
	        /// </summary>
	        /// <returns>A list of junction SnakeNodes</returns>
	        public ArrayList<SnakeNode> findJunctionNeighbours()
	        {
	            ArrayList<SnakeNode> junctionNeighbours = new ArrayList<SnakeNode>();

	            for (SnakeNode neighbour : this.neighbours)
	            {
	                SnakeNode previousNode = this;
	                SnakeNode nextNode = neighbour;

	                while (nextNode.neighbours.size() == 2)
	                {
	                    if (nextNode.neighbours.get(0) == previousNode)
	                    {
	                        previousNode = nextNode;
	                        nextNode = nextNode.neighbours.get(1);
	                    }
	                    else
	                    {
	                        previousNode = nextNode;
	                        nextNode = nextNode.neighbours.get(0);
	                    }
	                }   
	                junctionNeighbours.add(nextNode);
	            }
	            return junctionNeighbours;
	        }
	        
	      /// Find a wall linking this node to a destination node
	        /// </summary>
	        /// <param name="destination">The snakenode that is the target of this search</param>
	        /// <returns>A list of nodes from the current snake node, up to the destination node, if such a wall exists.</returns>
	        public ArrayList<SnakeNode> findWall(SnakeNode destination)
	        {
	            ArrayList<SnakeNode> currentWall = new ArrayList<SnakeNode>();

	            for (SnakeNode neighbour : this.neighbours)
	            {
	                if (neighbour == destination)
	                {
	                    currentWall.add(this);
	                    currentWall.add(neighbour);
	                    return currentWall;
	                }

	                SnakeNode currentNode = this;
	                SnakeNode nextNode = neighbour;

	                while (nextNode.neighbours.size() == 2)
	                {
	                    currentWall.add(currentNode);
	                    if (nextNode.neighbours.get(0) == currentNode)
	                    {
	                        currentNode = nextNode;
	                        nextNode = nextNode.neighbours.get(1);
	                    }
	                    else
	                    {
	                        currentNode = nextNode;
	                        nextNode = nextNode.neighbours.get(0);
	                    }
	                }

	                if (nextNode == destination)
	                {
	                    currentWall.add(nextNode);
	                    return currentWall;
	                }
	                else
	                {
	                    currentWall.clear();
	                }
	            }
	            return null;
	        }
	        
	        /// Resets the neighbour links for a given node
	        /// </summary>
	        /// <param name="value">Resets to either true or false, depending on this value</param>
	        public void resetNeighbourLink(boolean value)
	        {
	            this.neighbourLink = new ArrayList<Boolean>();
	            int count = this.neighbours.size();
	            while (count > 0)
	            {
	                this.neighbourLink.add(value);
	                count--;
	            }
	        }

	    }
	 
	 public static class ExtentionMath
	    {
	        /// <summary>
	        /// Clamps a value into a specified range.
	        /// </summary>
	        /// <param name="val">The value to be clamped.</param>
	        /// <param name="lowerBound">The lower bound at which to clamp the value.</param>
	        /// <param name="upperBound">The upper bound at which to clamp the value.</param>
	        /// <returns></returns>
	        public static int clamp(int val, int lowerBound, int upperBound)
	        {
	            return (val > upperBound) ? upperBound : (val < lowerBound ? lowerBound : val);
	        }
	    }
	
	public class Metadata
    {
        private String title = null;
        private String instanceTitle = null;
        private ArrayList<HashMap<String, String>> data = new ArrayList<HashMap<String, String>>();
        public Metadata(String title, String instanceTitle)
        {
            this.title = title;
            this.instanceTitle = instanceTitle;
        }

        public void add(HashMap<String, String> item)
        {
            data.add(item);
        }

        public void clear()
        {
            data.clear();
        }

        public ArrayList<HashMap<String, String>> getMetadata()
        {
            return data;
        }
        
        public String getTitle() {
        	return title;
        }
        
        public void setTitle(String title) {
        	this.title = title;
        }
        
        public String getInstanceTitle() {
        	return instanceTitle;
        }
        
        public void setInstanceTitle(String instanceTitle) {
        	this.instanceTitle = instanceTitle;
        }
    }

    public interface IMetadataProvider
    {
    	Metadata getData();
    	void setData(Metadata data);
        void InitialiseData();
    }
}