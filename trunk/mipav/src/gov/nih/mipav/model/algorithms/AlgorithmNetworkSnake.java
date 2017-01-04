package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.*;

import java.awt.Point;
import java.awt.geom.Point2D;
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
	        
	        public void Initialise(SnakeInitialiser snakeInitialiser, int spacing)
	        {
	            // Set spacing
	            this.spacing = spacing;

	            // Create temporary sorted list to speed up processing
	            TreeMap<tuple2i, SnakeNode> sortingList = new TreeMap<tuple2i, SnakeNode>();

	            for (ArrayList<Point> nodePoints : snakeInitialiser.wallPositions)
	            {
	                ArrayList<Point> redistributedNodePoints = createAndRedistributePoints(nodePoints, spacing);

	                // Starting junction node
	                SnakeNode startNode = null;
	                tuple2i startKey = new tuple2i((int)redistributedNodePoints.get(0).x, (int)redistributedNodePoints.get(0).y);
	                if (sortingList.containsKey(startKey))
	                    startNode = sortingList.get(startKey);
	                else
	                {
	                    startNode = new SnakeNode();
	                    startNode.x = (int)redistributedNodePoints.get(0).x;
	                    startNode.y = (int)redistributedNodePoints.get(0).y;
	                    sortingList.put(startKey, startNode);
	                }

	                // Ending junction node
	                SnakeNode endNode = null;
	                tuple2i endKey = new tuple2i((int)redistributedNodePoints.get(redistributedNodePoints.size()-1).x,
	                		(int)redistributedNodePoints.get(redistributedNodePoints.size()-1).y);
	                if (sortingList.containsKey(endKey))
	                    endNode = sortingList.get(endKey);
	                else
	                {
	                    endNode = new SnakeNode();
	                    endNode.x = (int)redistributedNodePoints.get(redistributedNodePoints.size()-1).x;
	                    endNode.y = (int)redistributedNodePoints.get(redistributedNodePoints.size()-1).y;
	                    sortingList.put(endKey, endNode);
	                }

	                if (redistributedNodePoints.size() == 2)
	                {
	                    startNode.neighbours.add(endNode);
	                    endNode.neighbours.add(startNode);
	                }
	                else
	                {
	                    SnakeNode previousNode = startNode;
	                    for (int i = 1; i < redistributedNodePoints.size() - 1; i++)
	                    {
	                        int cX = (int)redistributedNodePoints.get(i).x, cY = (int)redistributedNodePoints.get(i).y;
	                        tuple2i currentKey = new tuple2i(cX, cY);
	                        SnakeNode currentNode = new SnakeNode();
	                        currentNode.x = cX;
	                        currentNode.y = cY;

	                        if (sortingList.containsKey(currentKey))
	                            continue;
	                        else
	                            sortingList.put(currentKey, currentNode);

	                        // Link nodes
	                        previousNode.neighbours.add(currentNode);
	                        currentNode.neighbours.add(previousNode);
	                        previousNode = currentNode;
	                    }
	                    // Link final nodes
	                    previousNode.neighbours.add(endNode);
	                    endNode.neighbours.add(previousNode);
	                }
	            }

	            // Copy all nodes into the final snakes list - after this point ordering is not a concern
	            Set set = sortingList.entrySet();
	            Iterator iterator = set.iterator();
	            while (iterator.hasNext()) {
	            	Map.Entry mentry = (Map.Entry)iterator.next();
	            	this.nodes.add((SnakeNode)mentry.getValue());
	            }
	           

	            this.isInitialised = true;
	        }
	        
	        public void iterate(byte[] imageBuffer, int bufferWidth, int bufferHeight, int m, int timeout)
	        {

	            if (!this.isInitialised) {
	                MipavUtil.displayError("Cannot iterate a network snake that has not been initialised");
	                return;
	            }

	            // Iterate the network snake
	            float[] distances = new float[this.nodes.size()];

	            for (int i = 0; i < this.nodes.size(); i++)
	            {
	                distances[i] = this.nodes.get(i).findAverageDistance();
	            }

	            long startTime = System.currentTimeMillis();

	            //int iterationCount = 0;
	            double movedThreshold = 0.02;
	            int movedCount;
	            do
	            {
	                movedCount = 0;
	                for (int i = 0; i < this.nodes.size(); i++)
	                {
	                    if (this.nodes.get(i).iterate(this.alpha, this.beta, this.gamma, distances[i], imageBuffer, bufferWidth, bufferHeight, m))
	                        movedCount++;
	                }
	                //iterationCount++;
	            } while ((movedCount / (double)this.nodes.size() > movedThreshold) && ((System.currentTimeMillis() - startTime) < timeout));

	            this.data.clear();
	            HashMap<String, String> dataValues = new HashMap<String, String>();
	            
	            dataValues.put( "spacing", String.valueOf(spacing) );
	            dataValues.put( "curvature", String.valueOf(alpha) );
	            dataValues.put( "continuity", String.valueOf(beta) );
	            dataValues.put( "image", String.valueOf(gamma) );
	            
	            this.data.add(dataValues);
	        }
	        
	        public ArrayList<Point> createAndRedistributePoints(ArrayList<Point> originalPoints, int recordRate)
	        {
	            double[] cumulativeArray = new double[originalPoints.size()];
	            for (int i = 1; i < cumulativeArray.length; i++)
	            {
	                double distance = Math.sqrt(Math.pow(originalPoints.get(i - 1).x - originalPoints.get(i).x, 2.0) + 
	                		Math.pow(originalPoints.get(i - 1).y - originalPoints.get(i).y, 2.0));
	                cumulativeArray[i] = cumulativeArray[i - 1] + distance;
	            }
	            double totalLength = cumulativeArray[cumulativeArray.length-1];

	            for (int i = 1; i < cumulativeArray.length; i++)
	            {
	                cumulativeArray[i] /= totalLength;
	            }

	            int optimalPointCount = (int)(totalLength / recordRate) + 2;

	            ArrayList<Point> redistributedPoints = new ArrayList<Point>();
	            for (int i = 0; i < optimalPointCount; i++)
	            {
	                if (i == 0)
	                {
	                    redistributedPoints.add(originalPoints.get(0));
	                }
	                else if (i < optimalPointCount - 1)
	                {
	                    // Calculate t between 0 and 1
	                    double t = i / ((double)optimalPointCount - 1);
	                    int cumulativeIndex = 0;
	                    while (cumulativeArray[cumulativeIndex] < t)
	                        cumulativeIndex++;

	                    double subt = (t - cumulativeArray[cumulativeIndex - 1]) / (cumulativeArray[cumulativeIndex] - cumulativeArray[cumulativeIndex - 1]);

	                    Point P1 = originalPoints.get(cumulativeIndex - 1);
	                    Point P2 = originalPoints.get(cumulativeIndex);

	                    Point redistributedPoint = new Point((int)(P1.x * (1 - subt) + P2.x * (subt)), (int)(P1.y * (1 - subt) + P2.y * (subt)));
	                    redistributedPoints.add(new Point(redistributedPoint.x, redistributedPoint.y));
	                }
	                else if (i == optimalPointCount - 1)
	                {
	                    redistributedPoints.add(originalPoints.get(originalPoints.size()-1));
	                }
	            }
	            return redistributedPoints;
	        }


	}
	
	public class ContractingSnake
    {
        private boolean isInitialised = false;
        public boolean getIsInitialised()
        {
            return isInitialised;
        }

        private ArrayList<ContractingSnakeNode> nodes = new ArrayList<ContractingSnakeNode>();
        public ArrayList<ContractingSnakeNode> getNodes() {
        	return nodes;
        }
        public void setNodes(ArrayList<ContractingSnakeNode> nodes) {
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
        public float getGamma() {
        	return gamma;
        }
        public void setGamma(float gamma) {
        	this.gamma = gamma;
        }

        public ContractingSnake()
        {
        }

        //public void Initialise(IEnumerable<Point> snakePoints)
        public void initialize(ArrayList<Point> snakePoints)
        {
            ContractingSnakeNode previousNode = null;
            for (Point p : snakePoints)
            {
                ContractingSnakeNode currentNode = new ContractingSnakeNode();
                currentNode.x = (int)p.x;
                currentNode.y = (int)p.y;

                if (previousNode != null)
                {
                    currentNode.neighbours.add(previousNode);
                    previousNode.neighbours.add(currentNode);
                }

                this.nodes.add(currentNode);
                previousNode = currentNode;
            }

            // Link the end nodes
            ContractingSnakeNode firstNode = nodes.get(0);
            ContractingSnakeNode lastNode = nodes.get(nodes.size()-1);
            firstNode.neighbours.add(lastNode);
            lastNode.neighbours.add(firstNode);

            this.isInitialised = true;
        }

        public void iterate(byte[] imageBuffer, int bufferWidth, int bufferHeight, int m)
        {
            if (!this.isInitialised) {
                MipavUtil.displayError("Cannot iterate a contracting snake that has not been initialised");
                return;
            }

            // Iterate the network snake
            int iterationCount = 0;
            double movedThreshold = 0.02;
            int movedCount;
            do
            {
                boolean imaging = iterationCount > 3;
                movedCount = 0;
                for (int i = 0; i < this.nodes.size(); i++)
                {
                    if (this.nodes.get(i).iterate(this.alpha, this.beta, this.gamma, 0, imageBuffer, bufferWidth, bufferHeight, m, imaging))
                        movedCount++;
                }
                iterationCount++;

            } while (movedCount / (double)this.nodes.size() > movedThreshold);
        }

    }
	
	 public class ContractingSnakeNode implements Comparable
	    {
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
	        

	        protected ArrayList<ContractingSnakeNode> neighbours;
	        public ArrayList<ContractingSnakeNode> getNeighbours() {
	        	return neighbours;
	        }
	        public void setNeighbours(ArrayList neighbours) {
	        	this.neighbours = neighbours;
	        }
	        

	        private ArrayList<Boolean> neighbourLink;
	        public ArrayList<Boolean> getNeighbourLink() {
	        	return neighbourLink;
	        }
	        public void setNeighbourLink(ArrayList<Boolean> neighbourLink) {
	        	this.neighbourLink = neighbourLink;
	        }
	        


	        public ContractingSnakeNode()
	        {
	            this.neighbours = new ArrayList<ContractingSnakeNode>();
	        }

	        public boolean iterate(float alpha, float beta, float gamma, float d, byte[] imageBuffer, int bufferWidth, int bufferHeight, int m, boolean imaging)
	        { 
	            // Record starting position
	            Point startingPosition = new Point(this.x, this.y);

	            if (this.neighbours.size() == 2)
	            {
	                ContractingSnakeNode neighbour0 = this.neighbours.get(0);
	                ContractingSnakeNode neighbour1 = this.neighbours.get(1);

	                if (neighbour0 == null || neighbour1 == null) {
	                    MipavUtil.displayError("Contracting snake node has incompatible or missing neighbour");
	                    return false;
	                }

	                // Calculate bounds
	                int xMin = Math.max(0, this.x - m);
	                int xMax = Math.min(this.x + m, bufferWidth - 1);
	                int yMin = Math.max(0, this.y - m);
	                int yMax = Math.min(this.y + m, bufferHeight - 1);

	                // Econt
	                float[][] Econt = new float[xMax - xMin + 1][ yMax - yMin + 1];
	                float EcontMax = -Float.MAX_VALUE;
	                int ypos = 0;
	                int x2 = neighbour0.x;
	                int y2 = neighbour0.y;
	                for (int y = yMin; y <= yMax; y++)
	                {
	                    int xpos = 0;
	                    for (int x = xMin; x <= xMax; x++)
	                    {
	                        // Altered continuity term should shrink the snake
	                        Econt[xpos][ ypos] = (float)Math.pow(x - x2, 2.0) + (float)Math.pow(y - y2, 2.0);
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
	                float EcurveMax = -Float.MAX_VALUE;
	                int x0 = neighbour0.x;
	                int y0 = neighbour0.y;
	                x2 = neighbour1.x;
	                y2 = neighbour1.y;
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
	                float EimageMax = -Float.MAX_VALUE;

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
	                    {
	                        Eimage[x][ y] = (Eimage[x][ y] - EimageMin) / Math.max(1, EimageMax - EimageMin);
	                        if (!imaging)
	                            Eimage[x][ y] = 1;
	                    }

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
	            else
	            {
	                MipavUtil.displayError("Contracting snake node does not have exactly two neighbours");
	                return false;
	            }

	            Point finalPosition = new Point(this.x, this.y);
	            return startingPosition != finalPosition;
	        }

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

	        public int compareTo(Object obj)
	        {
	            if (obj instanceof ContractingSnakeNode)
	            {
	                ContractingSnakeNode compare = (ContractingSnakeNode)obj;
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
	            MipavUtil.displayError("Object is not a ContractingSnakenode");
	            return Integer.MAX_VALUE;
	        }
	        
	        private int clamp(int val, int lowerBound, int upperBound)
	        {
	            return (val > upperBound) ? upperBound : (val < lowerBound ? lowerBound : val);
	        }
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
	        
	        public class KeyValuePair {
	    		public tuple2i key;
	    		public Node value;
	    		
	    		public KeyValuePair(tuple2i key, Node value) {
	    			this.key = key;
	    			this.value = value;
	    		}
	    		
	    	}
	        
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
	        
	        public void loadPixelGrid(PixelGrid pixelGrid)
	        {
	            int width = pixelGrid.getGridWidth();
	            int height = pixelGrid.getGridHeight();

	            // Create internal junctions
	            for (PixelGrid.PixelGridNode internalJunction : pixelGrid.junctions)
	            {
	                tuple2i key = new tuple2i(internalJunction.x, internalJunction.y);
	                Node junctionNode = new Node();
	                junctionNode.x = internalJunction.x;
	                junctionNode.y = internalJunction.y;
	                this.nodeList.put(key, junctionNode);
	            }

	            // Create external junctions
	            for (PixelGrid.PixelGridNode externalJunction : pixelGrid.edgeJunctions)
	            {
	                tuple2i key = new tuple2i(externalJunction.x, externalJunction.y);
	                Node junctionNode = new Node();
	                junctionNode.x = externalJunction.x;
	                junctionNode.y = externalJunction.y;

	                if (junctionNode.x == 0)
	                    junctionNode.anchor = AnchorPosition.West;
	                else if (junctionNode.x == width - 1)
	                    junctionNode.anchor = AnchorPosition.East;
	                else if (junctionNode.y == 0)
	                    junctionNode.anchor = AnchorPosition.North;
	                else if (junctionNode.y == height - 1)
	                    junctionNode.anchor = AnchorPosition.South;

	                this.nodeList.put(key, junctionNode);
	            }

	            // Find all walls
	            boolean[][] visitedPixels = new boolean[pixelGrid.getGridWidth()][ pixelGrid.getGridHeight()];
	            ArrayList<ArrayList<Point>> allWalls = new ArrayList<ArrayList<Point>>();
	            ArrayList<ArrayList<Point>> finalWalls = new ArrayList<ArrayList<Point>>();

	            Set set = this.nodeList.entrySet();
	            Iterator iterator = set.iterator();
	            while (iterator.hasNext()) {
	            	Map.Entry mentry = (Map.Entry)iterator.next();
	            	Node currentNode = (Node)mentry.getValue();
	            	ArrayList<Point>[] walls = pixelGrid.findBranches(currentNode.x, currentNode.y, this.recordRate, visitedPixels);

	                for (ArrayList<Point> wall : walls)
	                {
	                    if (wall != null)
	                        allWalls.add(wall);
	                }
	            }

	            for (int i = 0; i < allWalls.size(); i++)
	            {
	                ArrayList<Point> wall = redistributePoints(allWalls.get(i));
	                finalWalls.add(wall);

	                // Obtain reference to starting and finishing nodes
	                Point currentPoint = wall.get(0);
	                Node currentNode = this.nodeList.get(new tuple2i((int)currentPoint.x, (int)currentPoint.y));

	                Point lastPoint = wall.get(wall.size()-1);
	                Node lastNode = this.nodeList.get(new tuple2i((int)lastPoint.x, (int)lastPoint.y));

	                if (currentNode != null && lastNode != null)
	                {
	                    if (wall.size() == 2)
	                    {
	                        linkNodes(currentNode, lastNode);
	                    }
	                    else if (wall.size() == 3)
	                    {
	                        Point p = wall.get(1);
	                        tuple2i key = new tuple2i((int)p.x, (int)p.y);
	                        Node nextNode = new Node();
	                        nextNode.x = (int)p.x;
	                        nextNode.y = (int)p.y;
	                        linkNodes(currentNode, nextNode);
	                        linkNodes(nextNode, lastNode);
	                        this.nodeList.put(key, nextNode);
	                    }
	                    else
	                    {
	                        for (int j = 1; j < wall.size() - 1; j++)
	                        {
	                            Point p = wall.get(j);
	                            tuple2i key = new tuple2i((int)p.x, (int)p.y);
	                            Node nextNode = new Node();
	                            nextNode.x = (int)p.x;
	                            nextNode.y = (int)p.y;

	                            if (j < wall.size() - 2)
	                            {
	                                linkNodes(currentNode, nextNode);
	                                this.nodeList.put(key, nextNode);
	                                currentNode = nextNode;
	                            }
	                            else if (j < wall.size() - 1)
	                            {
	                                linkNodes(currentNode, nextNode);
	                                linkNodes(nextNode, lastNode);
	                                this.nodeList.put(key, nextNode);
	                            }
	                        }
	                    }
	                }
	            }

	            // Merge junction nodes that are close together
	            mergeNodes();

	            // Refresh lists
	            rePopulateLists();
	        }
	        
	        public void mergeNodes()
	        {
        	    Set set = this.nodeList.entrySet();
	            Iterator iterator = set.iterator();
	            while (iterator.hasNext()) {
	            	Map.Entry mentry = (Map.Entry)iterator.next();
	            	Node mergedNode = (Node)mentry.getValue();
	                do
	                {
	                    mergedNode = mergeNearbyJunctionNodes(mergedNode, this.mergeThreshold);
	                } while (mergedNode != null);
	            }
	        }

	        private Node mergeNearbyJunctionNodes(Node currentNode, double minimumJunctionDistance)
	        {
	            if (currentNode.getNeighbourCount() > 2) // Discount external and wall nodes
	            {
	                for (SnakeInitialiser.Node neighbour : currentNode.neighbours)
	                {
	                    if (neighbour.getNeighbourCount() > 2) // Discount external and wall nodes for neighbours
	                    {
	                        if (Math.sqrt(Math.pow(currentNode.x - neighbour.x, 2) + Math.pow(currentNode.y - neighbour.y, 2)) < minimumJunctionDistance)
	                        {
	                            // Merge nodes - currentNode and Neighbour
	                            // Create new node at the average position of the previous two
	                            int newX = (int)((currentNode.x + neighbour.x) / 2 + 0.5);
	                            int newY = (int)((currentNode.y + neighbour.y) / 2 + 0.5);
	                            tuple2i newKey = new tuple2i(newX, newY);
	                            Node newNode = new Node();
	                            newNode.x = newX; newNode.y = newY;

	                            // Unlink currentNode and Neighbour
	                            this.unLinkNodes(currentNode, neighbour);

	                            // Remove all references to currentNode and neighbour
	                            for (Node n : currentNode.neighbours)
	                            {
	                                n.neighbours.remove(currentNode);
	                            }
	                            for (Node n : neighbour.neighbours)
	                            {
	                                n.neighbours.remove(neighbour);
	                            }

	                            // Link all neighbours to new node
	                            for (Node n : currentNode.neighbours)
	                            {
	                                this.linkNodes(newNode, n);
	                            }
	                            for (Node n : neighbour.neighbours)
	                            {
	                                this.linkNodes(newNode, n);
	                            }

	                           

	                            // Add new node
	                            if (this.nodeList.containsKey(newKey))
	                            {
	                                // If new node position already exists, attempt to offset in order to merge next time
	                                ArrayList<tuple2i> alternateKeys = new ArrayList<tuple2i>();
	                                
	                                alternateKeys.add(new tuple2i (newX-1, newY));
	                                alternateKeys.add(new tuple2i (newX+1, newY));
	                                alternateKeys.add(new tuple2i (newX, newY-1));
	                                alternateKeys.add(new tuple2i (newX, newY+1));
	                                alternateKeys.add(new tuple2i (newX-1, newY-1));
	                                alternateKeys.add(new tuple2i (newX-1, newY+1));
	                                alternateKeys.add(new tuple2i (newX+1, newY-1));
	                                alternateKeys.add(new tuple2i (newX+1, newY+1));
	                              

	                                for (tuple2i alternateKey : alternateKeys)
	                                {
	                                    if (!this.nodeList.containsKey(alternateKey))
	                                    {
	                                        newNode.x = alternateKey.a;
	                                        newNode.y = alternateKey.b;
	                                        // Remove currentNode and neighbour
	                                        this.nodeList.remove(new tuple2i(currentNode.x, currentNode.y));
	                                        this.nodeList.remove(new tuple2i(neighbour.x, neighbour.y));
	                                        this.nodeList.put(alternateKey, newNode);
	                                        return newNode;
	                                    }
	                                }

	                                // Alternate location search has failed, do not merge these nodes
	                                return null;
	                            }
	                            else
	                            {
	                                // Remove currentNode and neighbour
	                                this.nodeList.remove(new tuple2i(currentNode.x, currentNode.y));
	                                this.nodeList.remove(new tuple2i(neighbour.x, neighbour.y));
	                                this.nodeList.put(newKey, newNode);
	                            }

	                            // Do not attempt to merge with currentNode again - but return the new node to be merged if necessary
	                            return newNode;
	                        }
	                    }
	                }
	            }
	            return null;
	        }
	        
	        public ArrayList<Point> createAndRedistributePoints(ArrayList<Point> originalPoints)
	        {
	            double[] cumulativeArray = new double[originalPoints.size()];
	            for (int i = 1; i < cumulativeArray.length; i++)
	            {
	                double distance = Math.sqrt(Math.pow(originalPoints.get(i - 1).x - originalPoints.get(i).x, 2.0) +
	                		Math.pow(originalPoints.get(i - 1).y - originalPoints.get(i).y, 2.0));
	                cumulativeArray[i] = cumulativeArray[i - 1] + distance;
	            }
	            double totalLength = cumulativeArray[cumulativeArray.length-1];

	            for (int i = 1; i < cumulativeArray.length; i++)
	            {
	                cumulativeArray[i] /= totalLength;
	            }

	            int optimalPointCount = (int)(totalLength / this.recordRate) + 2;

	            ArrayList<Point> redistributedPoints = new ArrayList<Point>();
	            for (int i = 0; i < optimalPointCount; i++)
	            {
	                if (i == 0)
	                {
	                    redistributedPoints.add(originalPoints.get(0));
	                }
	                else if (i < optimalPointCount - 1)
	                {
	                    // Calculate t between 0 and 1
	                    double t = i / ((double)optimalPointCount - 1);
	                    int cumulativeIndex = 0;
	                    while (cumulativeArray[cumulativeIndex] < t)
	                        cumulativeIndex++;

	                    double subt = (t - cumulativeArray[cumulativeIndex - 1]) / (cumulativeArray[cumulativeIndex] - cumulativeArray[cumulativeIndex - 1]);

	                    Point P1 = originalPoints.get(cumulativeIndex - 1);
	                    Point P2 = originalPoints.get(cumulativeIndex);

	                    Point redistributedPoint = new Point((int)(P1.x * (1 - subt) + P2.x * (subt)), (int)(P1.y * (1 - subt) + P2.y * (subt)));
	                    redistributedPoints.add(new Point(redistributedPoint.x, redistributedPoint.y));
	                }
	                else if (i == optimalPointCount - 1)
	                {
	                    redistributedPoints.add(originalPoints.get(originalPoints.size()-1));
	                }
	            }
	            return redistributedPoints;
	        }
	        
	        public ArrayList<Point> redistributePoints(ArrayList<Point> originalPoints)
	        {
	            double[] cumulativeArray = new double[originalPoints.size()];
	            for (int i = 1; i < cumulativeArray.length; i++)
	            {
	                double distance = Math.sqrt(Math.pow(originalPoints.get(i-1).x - originalPoints.get(i).x,2.0) + 
	                		Math.pow(originalPoints.get(i-1).y - originalPoints.get(i).y,2.0));
	                cumulativeArray[i] = cumulativeArray[i - 1] + distance;
	            }
	            for (int i = 1; i < cumulativeArray.length; i++)
	            {
	                cumulativeArray[i] /= cumulativeArray[cumulativeArray.length-1];
	            }

	            ArrayList<Point> redistributedPoints = new ArrayList<Point>();
	            for (int i = 0; i < originalPoints.size(); i++)
	            {
	                if (i == 0)
	                {
	                    redistributedPoints.add(originalPoints.get(0));
	                }
	                else if (i < originalPoints.size() - 1)
	                {
	                    // Calculate t between 0 and 1
	                    double t = i / ((double)originalPoints.size() - 1);
	                    int cumulativeIndex = 0;
	                    while (cumulativeArray[cumulativeIndex] < t)
	                        cumulativeIndex++;

	                    double subt = (t - cumulativeArray[cumulativeIndex-1]) / (cumulativeArray[cumulativeIndex] - cumulativeArray[cumulativeIndex-1]);

	                    Point P1 = originalPoints.get(cumulativeIndex - 1);
	                    Point P2 = originalPoints.get(cumulativeIndex);

	                    Point redistributedPoint = new Point((int)(P1.x * (1 - subt) + P2.x * (subt)), (int)(P1.y * (1 - subt) + P2.y * (subt)));
	                    redistributedPoints.add(new Point (redistributedPoint.x,redistributedPoint.y));
	                }
	                else if (i == originalPoints.size() - 1)
	                {
	                    redistributedPoints.add(originalPoints.get(originalPoints.size()-1));
	                }
	            }
	            return redistributedPoints;
	        }
	        
	        public void addNode(int x, int y, AnchorPosition anchor)
	        {
	            Node additionNode = new Node();
	            additionNode.x = x;
	            additionNode.y = y;
	            additionNode.anchor = anchor;
	            this.nodeList.put(new tuple2i(x, y), additionNode);
	        }
	        
	        public Node insertNode(int x, int y, AnchorPosition anchor, int ax, int ay, int bx, int by)
	        {
	            Node additionNode = new Node();
	            additionNode.x = x;
	            additionNode.y = y;
	            additionNode.anchor = anchor;

	            tuple2i aKey = new tuple2i(ax, ay);
	            tuple2i bKey = new tuple2i(bx, by);

	            Node a = this.nodeList.get(aKey);
	            Node b = this.nodeList.get(bKey);

	            unLinkNodes(a, b);
	            linkNodes(a, additionNode);
	            linkNodes(additionNode, b);

	            this.nodeList.put(new tuple2i(x, y), additionNode);
	            return additionNode;
	        }
	        
	        public Node insertNode(int x, int y, AnchorPosition anchor, int ax, int ay)
	        {
	            Node additionNode = new Node();
	            additionNode.x = x;
	            additionNode.y = y;
	            additionNode.anchor = anchor;

	            tuple2i aKey = new tuple2i(ax, ay);

	            Node a = this.nodeList.get(aKey);

	            linkNodes(a, additionNode);

	            this.nodeList.put(new tuple2i(x, y), additionNode);
	            return additionNode;
	        }
	        
	        public void removeNode(int x, int y, boolean Unlink)
	        {
	            tuple2i removalKey = new tuple2i(x,y);
	            Node removalNode = this.nodeList.get(removalKey);
	            if (Unlink)
	            {
	                for (Node neighbour : removalNode.neighbours)
	                {
	                    neighbour.neighbours.remove(removalNode);
	                }
	            }
	            this.nodeList.remove(removalKey);
	        }
	        
	        public void linkNodes(Node node1, Node node2)
	        {
	            if (node1 != null && node2 != null)
	            {
	                if(node1.neighbours.indexOf(node2) < 0)
	                {
	                    node1.neighbours.add(node2);
	                }
	                if (node2.neighbours.indexOf(node1) < 0)
	                {
	                    node2.neighbours.add(node1);
	                }
	            }
	        }
	        
	        public void linkJunctionNodes(Node node1, Node node2)
	        {
	            if (node1 != null && node2 != null)
	            {
	                //if (node1.JunctionNeighbours.IndexOf(node2) < 0)
	                //{
	                    node1.junctionNeighbours.add(node2);
	                //}
	                //if (node2.JunctionNeighbours.IndexOf(node1) < 0)
	                //{
	                    node2.junctionNeighbours.add(node1);
	                //}
	            }
	        }
	        
	        public void unLinkNodes(Node node1, Node node2)
	        {
	            if (node1 != null && node2 != null)
	            {
	                node1.neighbours.remove(node2);
	                node2.neighbours.remove(node1);
	            }
	        }
	        
	        public void refresh()
	        {
	            rePopulateLists();
	        }
	    
	        private void rePopulateLists()
	        {
	            long DTStart = System.currentTimeMillis();
	            if (this.internalNodes == null)
	                this.internalNodes = new ArrayList<Node>();
	            this.internalNodes.clear();

	            if (this.externalNodes == null)
	                this.externalNodes = new ArrayList<Node>();
	            this.externalNodes.clear();

	            if (this.linkingNodes == null)
	                this.linkingNodes = new ArrayList<Node>();
	            this.linkingNodes.clear();
	            
	            Set set = this.nodeList.entrySet();
	            Iterator iterator = set.iterator();
	            while (iterator.hasNext()) {
	            	Map.Entry mentry = (Map.Entry)iterator.next();
	            	Node n = (Node)mentry.getValue();
		            if (n.anchor == AnchorPosition.None)
	                {
	                    if (n.getNeighbourCount() > 2)
	                        this.internalNodes.add(n);
	                    else
	                        this.linkingNodes.add(n);
	                }
	                else
	                {
	                    this.externalNodes.add(n);
	                }
	            }

	            long DTMid1 = System.currentTimeMillis();
	            long ts = DTMid1 - DTStart;
	            System.out.println("List Junction Nodes: " + ts + " milliseconds");

	            this.reCalculateJunctionNeighbours();

	            long DTMid2 = System.currentTimeMillis();
	            ts = DTMid2 - DTMid1;
	            System.out.println("Junction neighbours: " + ts + " milliseconds");
	            
	            this.wallPositions = getAllWalls(this.internalNodes, this.externalNodes);

	            long DTMid3 = System.currentTimeMillis();
	            ts = DTMid3 - DTMid2;
	            System.out.println("Wall positions: " + ts + " milliseconds");

	            this.reCalculateJunctionNeighbours();

	            ts = System.currentTimeMillis() - DTMid3;
	            System.out.println("Junction neighbours 2: " + ts + " milliseconds");

	            fixConsistency();
	        }
	        
	        private void fixConsistency()
	        {
	            ArrayList<KeyValuePair> removeList = new ArrayList<KeyValuePair>();

	            Set set = this.nodeList.entrySet();
	            Iterator iterator = set.iterator();
	            while (iterator.hasNext()) {
	            	Map.Entry mentry = (Map.Entry)iterator.next();
	            	tuple2i key = (tuple2i)mentry.getKey();
	            	Node n = (Node)mentry.getValue();
	            	if (key.a != n.x || key.b != n.y) {
	                    MipavUtil.displayError("Keys and node values not identical in SnakeInitialiser");
	                    return;
	            	}

	                if (n.getNeighbourCount() == 0 || (n.getNeighbourCount() == 1 && n.anchor == AnchorPosition.None))
	                {
	                    // Internal stub node should be removed
	                    if (n.getNeighbourCount() == 0)
	                        this.nodeList.remove(key);
	                    else if (n.getNeighbourCount() == 1)
	                    {
	                        removeList.add(new KeyValuePair(key, n));
	                    }
	                }
	            }
	            

	            // Remove all internal stub nodes
	            for (int i = 0; i < removeList.size(); i++)
	            {
	            	KeyValuePair kvp = removeList.get(i);
	                Node n = kvp.value;
	                tuple2i key = kvp.key;
	                Node nextNode = n.neighbours.get(0);
	                nextNode.neighbours.remove(n);
	                nextNode.junctionNeighbours.remove(n);
	                this.nodeList.remove(key);
	            }

	            if (removeList.size() != 0)
	            {
	                MipavUtil.displayWarning("One or more erroneous internal snake nodes were found and removed. "
	                		+ "This could be an indication that there is too much noise in the segmented image.");
	            }

	        }
	        
	        private ArrayList<ArrayList<Point>> getAllWalls(ArrayList<Node> internalNodes, ArrayList<Node> externalNodes)
	        {
	            ArrayList<ArrayList<Point>> walls = new ArrayList<ArrayList<Point>>();

	            int internalNodeCount = internalNodes.size();
	            for (int i = 0; i < internalNodeCount; i++)
	            {
	                getJunctionWalls(internalNodes.get(i), walls);
	            }

	            int externalNodeCount = externalNodes.size();
	            for (int i = 0; i < externalNodeCount; i++)
	            {
	                getJunctionWalls(externalNodes.get(i), walls);
	            }

	            return walls;
	        }

	        private void getJunctionWalls(Node junction, ArrayList<ArrayList<Point>> walls)
	        {
	            for (int i = 0; i < junction.junctionNeighbours.size(); i++)
	            {
	                if (junction.junctionNeighbours.get(i) != null)
	                    walls.add(getWall(junction, i));
	            }
	        }
	        
	        private ArrayList<Point> getWall(Node sourceNode, int index)
	        {
	            Node currentNode = sourceNode;

	            ArrayList<Point> wallPoints = new ArrayList<Point>();
	            wallPoints.add(new Point(currentNode.x, currentNode.y));

	            Node nextNode = currentNode.neighbours.get(index);
	            while (nextNode.getNeighbourCount() == 2)
	            {
	                wallPoints.add(new Point(nextNode.x, nextNode.y));
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

	            wallPoints.add(new Point(nextNode.x, nextNode.y));

	            sourceNode.junctionNeighbours.set(index,null);
	            for (int i = 0; i < nextNode.neighbours.size(); i++)
	            {
	                if (nextNode.neighbours.get(i) == currentNode)
	                {
	                    nextNode.junctionNeighbours.set(i,null);
	                }
	            }

	            return wallPoints;
	        }
	        
	        public void RemoveWalls(Node currentNode)
	        {
	            removeWall(currentNode, null);
	            this.rePopulateLists();
	        }

	        private void removeWall(Node currentNode, Node sourceNode)
	        {
	            ArrayList<Node> endPoints = new ArrayList<Node>();
	            // Find next node along and initiate removal
	            for (int i = 0; i < currentNode.neighbours.size(); i++)
	            {
	                Node nextNode = currentNode.neighbours.get(i);
	                if (nextNode != sourceNode && nextNode.getNeighbourCount() < 3)
	                {
	                    removeWall(nextNode, currentNode);
	                }
	                else if (nextNode != sourceNode && nextNode.getNeighbourCount() >= 3)
	                {
	                    endPoints.add(nextNode);
	                }
	            }

	            for (Node n : endPoints)
	            {
	                unLinkNodes(currentNode, n);
	            }

	            this.removeNode(currentNode.x, currentNode.y, false);
	        }
	        
	        public ArrayList<Node> findNextJunctions(Node currentNode)
	        {
	            ArrayList<Node> foundJunctions = new ArrayList<Node>();

	            for (Node n : currentNode.neighbours)
	            {
	                if (n.getNeighbourCount() != 2)
	                    foundJunctions.add(n);
	                else
	                {
	                    Node nextJunction = findNextJunction(n, currentNode);
	                    if (nextJunction != null)
	                        foundJunctions.add(nextJunction);
	                }
	            }
	            return foundJunctions;
	        }

	        private Node findNextJunction(Node currentNode, Node sourceNode)
	        {
	            for (Node n : currentNode.neighbours)
	            {
	                if (n != sourceNode)
	                {
	                    if (n.getNeighbourCount() != 2)
	                        return n;
	                    else
	                        return findNextJunction(n, currentNode);
	                }
	            }
	            return null;
	        }
	        
	        public void addLineSegment(Node StartNodeA, Node StartNodeB, Node EndNodeA, Node EndNodeB, ArrayList<Point> LineSegments)
	        {
	            // Create and redistribute points in along the user line.
	            ArrayList<Point> createdPoints = this.createAndRedistributePoints(LineSegments);

	            // Start Node
	            SnakeInitialiser.Node startNode = null;
	            if (StartNodeB == null)
	            {
	                // Insert joined to A
	                startNode = StartNodeA;
	            }
	            else
	            {
	                // Insert between A and B
	                SnakeInitialiser.Node SA = StartNodeA;
	                SnakeInitialiser.Node SB = StartNodeB;

	                int x = (int)createdPoints.get(0).x;
	                int y = (int)createdPoints.get(0).y;

	                if (this.exists(x, y))
	                {
	                    if (pointDistance(new Point(x, y), new Point(SA.x, SA.y)) < pointDistance(new Point(x, y), new Point(SB.x, SB.y)))
	                    {
	                        startNode = SA;
	                        createdPoints.set(0,new Point(SA.x, SA.y));
	                    }
	                    else
	                    {
	                        startNode = SB;
	                        createdPoints.set(0,new Point(SB.x, SB.y));
	                    }
	                }
	                else
	                {
	                    startNode = this.insertNode(x, y, AnchorPosition.None, SA.x, SA.y, SB.x, SB.y);
	                }
	            }


	            // End Node
	            SnakeInitialiser.Node endNode = null;
	            if (EndNodeB == null)
	            {
	                // Insert joined to A
	                endNode = EndNodeA;
	            }
	            else
	            {
	                // Insert between A and B
	                SnakeInitialiser.Node EA = EndNodeA;
	                SnakeInitialiser.Node EB = EndNodeB;

	                int x = (int)createdPoints.get(createdPoints.size()-1).x;
	                int y = (int)createdPoints.get(createdPoints.size()-1).y;

	                if (this.exists(x, y))
	                {
	                    if (pointDistance(new Point(x, y), new Point(EA.x, EA.y)) < pointDistance(new Point(x, y), new Point(EB.x, EB.y)))
	                    {
	                        endNode = EA;
	                        createdPoints.set(createdPoints.size() - 1,new Point(EA.x, EA.y));
	                    }
	                    else
	                    {
	                        endNode = EB;
	                        createdPoints.set(createdPoints.size() - 1,new Point(EB.x, EB.y));
	                    }
	                }
	                else
	                {
	                    endNode = this.insertNode((int)createdPoints.get(createdPoints.size()-1).x, (int)createdPoints.get(createdPoints.size()-1).y, AnchorPosition.None, EA.x, EA.y, EB.x, EB.y);
	                }
	            }

	            // Intermediate Nodes
	            SnakeInitialiser.Node currentNode = startNode;
	            for (int i = 1; i < createdPoints.size() - 1; i++)
	            {
	                int x = (int)createdPoints.get(i).x;
	                int y = (int)createdPoints.get(i).y;
	                if (!this.exists(x, y))
	                    currentNode = this.insertNode(x, y, AnchorPosition.None, currentNode.x, currentNode.y);
	                else
	                {
	                    SnakeInitialiser.Node n = this.nodeList.get(new tuple2i(x, y));
	                    this.linkNodes(currentNode, n);
	                    currentNode = n;
	                }
	            }

	            // Finish by linking to end node
	            if (endNode.anchor != AnchorPosition.None)
	            {
	                endNode = this.insertNode(endNode.x, endNode.y, endNode.anchor, currentNode.x, currentNode.y);
	            }
	            else
	            {
	                this.linkNodes(currentNode, endNode);
	            }
	            this.refresh();
	        }

	        private double pointDistance(Point P1, Point P2)
	        {
	            return Math.sqrt(Math.pow(P1.x - P2.x, 2.0) + Math.pow(P1.y - P2.y, 2.0));
	        }

	        private void reCalculateJunctionNeighbours()
	        {
	        	Set set = this.nodeList.entrySet();
	            Iterator iterator = set.iterator();
	            while (iterator.hasNext()) {
	            	Map.Entry mentry = (Map.Entry)iterator.next();
	                Node n = (Node)mentry.getValue();
	                n.junctionNeighbours.clear();
	                if (n.getNeighbourCount() != 2)
	                {
	                    ArrayList<Node> junctionNeighbours = findNextJunctions(n);
	                    for (Node neighbour : junctionNeighbours)
	                    {
	                        n.junctionNeighbours.add(neighbour);
	                        
	                        //LinkJunctionNodes(n, neighbour);
	                    }
	                }
	            }
	        }

	    } // public class SnakeInitialiser implements Serializable
	 
	 public class tuple2i {

		    public final int a;
		    public final int b;

		    public tuple2i(int a, int b) {
		        this.a = a;
		        this.b = b;
		    }

		}
	 
	 public class PixelGrid
	    {
	        public class PixelGridNode
	        {
	            private PixelGridNode north = null, south = null, east = null, west = null;
	            public PixelGridNode getNorth() {
	                return north;	
	            }
	            public void setNorth(PixelGridNode north) {
	            	this.north = north;
	            }
	            public PixelGridNode getSouth() {
	                return south;	
	            }
	            public void setSouth(PixelGridNode south) {
	            	this.south = south;
	            }
	            public PixelGridNode getEast() {
	                return east;	
	            }
	            public void setEast(PixelGridNode east) {
	            	this.east = east;
	            }
	            public PixelGridNode getWest() {
	                return west;	
	            }
	            public void setWest(PixelGridNode west) {
	            	this.west = west;
	            }
	            

	            private int x = 0, y = 0;
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
	           

	            public PixelGridNode(int x, int y, PixelGridNode North, PixelGridNode South, PixelGridNode East, PixelGridNode West)
	            {
	                this.north = North;
	                this.south = South;
	                this.east = East;
	                this.west = West;
	                this.x = x;
	                this.y = y;
	            }

	            public int getNeighbourCount()
	            {
	                    return (this.north != null ? 1 : 0) + (this.south != null ? 1 : 0) + (this.east != null ? 1 : 0) + (this.west != null ? 1 : 0);
	            }
	        }


	        private PixelGridNode[][] pixels;
	        public PixelGridNode[][] getPixels() {
	        	return pixels;
	        }
	        public void setPixels(PixelGridNode[][] pixels) {
	        	this.pixels = pixels;
	        }
	        

	        private ArrayList<PixelGridNode> junctions;
	        public ArrayList<PixelGridNode> getJunctions() {
	            return this.junctions;	
	        }

	        private ArrayList<PixelGridNode> edgeJunctions;
	        public ArrayList<PixelGridNode> getEdgeJunctions()
	        {
	            return this.edgeJunctions;
	        }

	        private int gridWidth = 0, gridHeight = 0;
	        public int getGridWidth()  {
	        	return this.gridWidth; 
	        }
	        public int getGridHeight() {
	        	return this.gridHeight;
	        }

	        public PixelGrid(int width, int height)
	        {
	            this.gridWidth = width;
	            this.gridHeight = height;
	            this.pixels = new PixelGridNode[width][ height];
	        }

	        //public void loadPixels(IEnumerable<Pixel> pixelList)
	        public void loadPixels (ArrayList<Pixel> pixelList)
	        {

	            for (Pixel p : pixelList)
	            {
	                setPixel(p.x, p.y);
	            }

	            findJunctions();
	            findEdgeJunctions();
	        }

	        private void findJunctions()
	        {
	            this.junctions = new ArrayList<PixelGridNode>();
	            for (int i = 0; i < pixels.length; i++) {
		            for (PixelGridNode pgn : this.pixels[i])
		            {
		                if (pgn != null)
		                    if (pgn.getNeighbourCount() > 2)
		                        junctions.add(pgn);
		            }
	            }
	        }

	        private void findEdgeJunctions()
	        {
	            this.edgeJunctions = new ArrayList<PixelGridNode>();
	            for (int i = 0; i < pixels.length; i++) {
		            for (PixelGridNode pgn : this.pixels[i])
		            {
		                if (pgn != null)
		                    if (pgn.getNeighbourCount() == 1)
		                        edgeJunctions.add(pgn);
		            }
	            }
	        }

	        public ArrayList<Point>[] findBranches(int x, int y, int recordRate, boolean[][] visitedPixels)
	        {
	            PixelGridNode sourceNode = this.pixels[x][ y];
	            if (sourceNode.getNeighbourCount() == 2)
	                return null;

	            ArrayList<Point>[] outputPoints = new ArrayList[4];
	            for (int i = 0; i < 4; i++) {
	            	outputPoints[i] = new ArrayList<Point>();
	            }

	            // North
	            if (sourceNode.north != null)
	            {
	                outputPoints[0] = traverseWall(sourceNode, sourceNode.north, recordRate, visitedPixels);
	            }

	            if (sourceNode.east != null)
	            {
	                outputPoints[1] = traverseWall(sourceNode, sourceNode.east, recordRate, visitedPixels);
	            }

	            if (sourceNode.south != null)
	            {
	                outputPoints[2] = traverseWall(sourceNode, sourceNode.south, recordRate, visitedPixels);
	            }

	            if (sourceNode.west != null)
	            {
	                outputPoints[3] = traverseWall(sourceNode, sourceNode.west, recordRate, visitedPixels);
	            }
	            return outputPoints;
	        }

	        private ArrayList<Point> traverseWall(PixelGridNode sourceNode, PixelGridNode nextNode, int recordRate, boolean[][] visitedPixels)
	        {
	            int visitedCount = 0;
	            PixelGridNode currentNode = nextNode;
	            PixelGridNode previousNode = sourceNode;
	            ArrayList<Point> outputPoints = new ArrayList<Point>();
	            outputPoints.add(new Point(previousNode.x, previousNode.y));
	            
	            // Detect wall of length 1 and return
	            if (nextNode.getNeighbourCount() != 2)
	            {
	                outputPoints.add(new Point(nextNode.x, nextNode.y));
	                return outputPoints;
	            }
	            
	            while (true)
	            {
	                // Make sure we haven't been down this wall already
	                if (visitedPixels[currentNode.x][ currentNode.y])
	                {
	                    outputPoints = null;
	                    break;
	                }
	                else
	                {
	                    visitedPixels[currentNode.x][ currentNode.y] = true;
	                }

	                // Do we need to record the current pixel
	                if (visitedCount == recordRate)
	                {
	                    // Record current pixel
	                    outputPoints.add(new Point(currentNode.x, currentNode.y));
	                    visitedCount = 0;
	                }

	                // Search for next neighbour and end if we are at a junction
	                if (currentNode.north != null && currentNode.north != previousNode)
	                {
	                    if (currentNode.north.getNeighbourCount() != 2)
	                    {
	                        outputPoints.add(new Point(currentNode.north.x, currentNode.north.y));
	                        break;
	                    }
	                    previousNode = currentNode;
	                    currentNode = currentNode.north;
	                }
	                else if (currentNode.east != null && currentNode.east != previousNode)
	                {
	                    if (currentNode.east.getNeighbourCount() != 2)
	                    {
	                        outputPoints.add(new Point(currentNode.east.x, currentNode.east.y));
	                        break;
	                    }
	                    previousNode = currentNode;
	                    currentNode = currentNode.east;
	                }
	                else if (currentNode.south != null && currentNode.south != previousNode)
	                {
	                    if (currentNode.south.getNeighbourCount() != 2)
	                    {
	                        outputPoints.add(new Point(currentNode.south.x, currentNode.south.y));
	                        break;
	                    }
	                    previousNode = currentNode;
	                    currentNode = currentNode.south;
	                }
	                else if (currentNode.west != null && currentNode.west != previousNode)
	                {
	                    if (currentNode.west.getNeighbourCount() != 2)
	                    {
	                        outputPoints.add(new Point(currentNode.west.x, currentNode.west.y));
	                        break;
	                    }
	                    previousNode = currentNode;
	                    currentNode = currentNode.west;
	                }
	                visitedCount++;
	            }
	            return outputPoints;
	        }


	        public void addPixelLine(int x1, int y1, int x2, int y2)
	        {
	            int dx = Math.abs(x2 - x1);
	            int dy = Math.abs(y2 - y1);
	            int sx = x1 < x2 ? 1 : -1;
	            int sy = y1 < y2 ? 1 : -1;
	            int err = dx - dy;

	            while (!(x1 == x2 && y1 == y2))
	            {
	                setPixel(x1, y1);
	                int e2 = 2 * err;
	                if (e2 > -dy)
	                {
	                    err = err - dy;
	                    x1 += sx;
	                }
	                
	                if (e2 < dx)
	                {
	                    err += dx;
	                    y1 += sy;
	                }

	                // Adaption to draw thicker lines - to avoid diagonal jumps
	                if (e2 > -dy && e2 < dx)
	                {
	                    if (sx > 0 && sy > 0)
	                        setPixel(x1-sx, y1);
	                    else if (sx < 0 && sy > 0)
	                        setPixel(x1, y1-sy);
	                    else if (sx > 0 && sy < 0)
	                        setPixel(x1-sx, y1);
	                    else if (sx < 0 && sy < 0)
	                        setPixel(x1, y1-sy);
	                }
	            }
	           
	            findJunctions();
	            findEdgeJunctions();
	        }

	        private void setPixel(int x, int y)
	        {
	            if (x < 0 || x >= this.gridWidth || y < 0 || y >= this.gridHeight)
	                return;

	            // North neighbour
	            PixelGridNode north = null;
	            if (y < this.gridHeight - 1)
	                north = this.pixels[x][ y + 1];

	            // South neighbour
	            PixelGridNode south = null;
	            if (y > 0)
	                south = this.pixels[x][ y - 1];

	            // East neighbour
	            PixelGridNode east = null;
	            if (x < this.gridWidth - 1)
	                east = this.pixels[x + 1][ y];

	            // West neighbour
	            PixelGridNode west = null;
	            if (x > 0)
	                west = this.pixels[x - 1][ y];

	            PixelGridNode current = new PixelGridNode(x, y, north, south, east, west);

	            // Link from neighbours to current
	            if (current.north != null)
	                current.north.south = current;

	            if (current.south != null)
	                current.south.north = current;

	            if (current.east != null)
	                current.east.west = current;

	            if (current.west != null)
	                current.west.east = current;

	            this.pixels[x][ y] = current;
	        }
	        
	    }
	 
	 //public structure Pixel
	 public class Pixel
	    {
	        /// <summary>
	        /// The x and y components
	        /// </summary>
	        public int x, y;

	        /// <summary>
	        /// Constructs a new <c>Pixel</c> given the specified coordinates
	        /// </summary>
	        /// <param name="x"></param>
	        /// <param name="y"></param>
	        public Pixel(int x, int y)
	        {
	            this.x = x;
	            this.y = y;
	        }

	        /// <summary>
	        /// Creates a string representation of the <c>Pixel</c> in the form "(X,Y)"
	        /// </summary>
	        /// <returns></returns>
	        @Override
	        public String toString()
	        {
	            StringBuilder sb = new StringBuilder();
	            sb.append('(');
	            sb.append(x);
	            sb.append(',');
	            sb.append(y);
	            sb.append(')');
	            return sb.toString();
	        }

	        /// <summary>
	        /// Compares pixel coordinates for equality
	        /// </summary>
	        /// <param name="obj">The object to query for equality</param>
	        /// <returns>True if the specified object is a pixel with the same coordinates as this, false otherwise</returns>
	        @Override
	        public boolean equals(Object obj)
	        {
	            if (obj == null) return false;
	            if (obj instanceof Pixel)
	            {
	                Pixel op = (Pixel)obj;

	                return x == op.x && y == op.y;
	            }
	            else
	            {
	                return super.equals(obj);
	            }
	        }

	        /// <summary>
	        /// Delegates to base
	        /// </summary>
	        /// <returns>See [base].GetHashCode() for this type</returns>
	        @Override
	        public int hashCode()
	        {
	            return super.hashCode();
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
	                float EcurveMax = -Float.MAX_VALUE;
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
	                float EimageMax = -Float.MAX_VALUE;

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
	                float EcontMax = -Float.MAX_VALUE;
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
	                float EcurveMax = -Float.MAX_VALUE;
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
	                float EimageMax = -Float.MAX_VALUE;

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
	                float EcurveMax = -Float.MAX_VALUE;

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
	                float EimageMax = -Float.MAX_VALUE;

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