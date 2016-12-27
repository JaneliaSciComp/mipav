package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.*;

import java.awt.Point;
import java.io.IOException;
import java.util.*;

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

	}
	
	 public class SnakeNode implements Comparable
	    {
	        /// The X and Y Positions of the snake node
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
	        public float FindAverageDistance()
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
	       /* public boolean Iterate(float alpha, float beta, float gamma, float d, byte[] imageBuffer, int bufferWidth, int bufferHeight, int m)
	        {
	            // Record starting position
	            Point startingPosition = new Point(this.x, this.y);

	            // Iterates the SnakeNode once, moving it to a nearby position that minimises internal and external energy
	            if (this.neighbours.Count == 1)
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

	                if (this.neighbours[0].Neighbours.Count == 2)
	                {
	                    // Find correct x2 and y2 neighbour
	                    if (this.neighbours[0].Neighbours[0] == this)
	                    {
	                        p2 = new Point( this.neighbours[0].Neighbours[1].X, this.neighbours[0].Neighbours[1].Y);
	                    }
	                    else
	                    {
	                        p2 = new Point( this.neighbours[0].Neighbours[0].X, this.neighbours[0].Neighbours[0].Y);
	                    }
	                }
	                else
	                {
	                    p2 = new Point( this.X, this.Y);
	                    useEcurve = false;
	                }
	                
	                int ypos = 0;
	                for (int y = yMin; y <= yMax; y++)
	                {
	                    int xpos = 0;
	                    for (int x = xMin; x <= xMax; x++)
	                    {
	                        Point p0 = new Point(x, y);
	                        Ecurve[xpos, ypos] = (float)((p0 - p1) - (p1 - p2)).LengthSquared;
	                        
	                        if (Ecurve[xpos, ypos] > EcurveMax)
	                            EcurveMax = Ecurve[xpos, ypos];
	                        xpos++;
	                    }
	                    ypos++;
	                }

	                // Normalise Ecurve
	                if (useEcurve)
	                {
	                    for (int y = 0; y < Ecurve.GetLength(1); y++)
	                        for (int x = 0; x < Ecurve.GetLength(0); x++)
	                            Ecurve[x, y] /= EcurveMax;
	                }
	                else
	                {
	                    // Immediately next to a junction, meaning curvature is impossible to calculate
	                    for (int y = 0; y < Ecurve.GetLength(1); y++)
	                        for (int x = 0; x < Ecurve.GetLength(0); x++)
	                            Ecurve[x, y] = 0.0f;
	                }

	                // Eimage
	                float[,] Eimage = new float[xMax - xMin + 1, yMax - yMin + 1];
	                float EimageMin = float.MaxValue;
	                float EimageMax = float.MinValue;

	                ypos = 0;
	                for (int y = yMin; y <= yMax; y++)
	                {
	                    int xpos = 0;
	                    for (int x = xMin; x <= xMax; x++)
	                    {
	                        Eimage[xpos, ypos] = 255 - imageBuffer[y * bufferWidth + x];

	                        if (Eimage[xpos, ypos] > EimageMax)
	                            EimageMax = Eimage[xpos, ypos];

	                        if (Eimage[xpos, ypos] < EimageMin)
	                            EimageMin = Eimage[xpos, ypos];
	                        xpos++;
	                    }
	                    ypos++;
	                }

	                // Normalise Eimage
	                for (int y = 0; y < Eimage.GetLength(1); y++)
	                    for (int x = 0; x < Eimage.GetLength(0); x++)
	                        Eimage[x, y] = (Eimage[x, y] - EimageMin) / Math.Max(1, EimageMax - EimageMin);

	                // Choose final position for snake point
	                float EMin = float.MaxValue;
	                int EminX = 0, EminY = 0;
	                ypos = 0;
	                for (int y = yMin; y <= yMax; y++)
	                {
	                    int xpos = 0;
	                    for (int x = xMin; x <= xMax; x++)
	                    {
	                        float E = beta * Ecurve[xpos, ypos] + gamma * Eimage[xpos, ypos];

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
	            else if (this.neighbours.Count == 2)
	            {
	                // Calculate bounds
	                int xMin = Math.Max(0, this.x - m);
	                int xMax = Math.Min(this.x + m, bufferWidth - 1);
	                int yMin = Math.Max(0, this.y - m);
	                int yMax = Math.Min(this.y + m, bufferHeight - 1);

	                // Econt
	                float[,] Econt = new float[xMax - xMin + 1, yMax - yMin + 1];
	                float EcontMax = float.MinValue;
	                int ypos = 0;
	                int x2 = this.Neighbours[0].x;
	                int y2 = this.Neighbours[0].y;
	                for (int y = yMin; y <= yMax; y++)
	                {
	                    int xpos = 0;
	                    for (int x = xMin; x <= xMax; x++)
	                    {
	                        Econt[xpos, ypos] = (float)Math.Pow(x - x2, 2.0) + (float)Math.Pow(y - y2, 2.0);
	                        Econt[xpos, ypos] = (float)Math.Sqrt(Econt[xpos, ypos]);
	                        Econt[xpos, ypos] = (float)Math.Pow(d - Econt[xpos, ypos],2.0);
	                        if (Econt[xpos, ypos] > EcontMax)
	                            EcontMax = Econt[xpos, ypos];
	                        xpos++;
	                    }
	                    ypos++;
	                }

	                // Normalise Econt
	                for (int y = 0; y < Econt.GetLength(1); y++)
	                    for (int x = 0; x < Econt.GetLength(0); x++)
	                        Econt[x, y] /= EcontMax;

	                // Ecurve
	                float[,] Ecurve = new float[xMax - xMin + 1, yMax - yMin + 1];
	                float EcurveMax = float.MinValue;
	                int x0 = this.neighbours[0].x;
	                int y0 = this.neighbours[0].y;
	                x2 = this.Neighbours[1].x;
	                y2 = this.Neighbours[1].y;
	                ypos = 0;
	                for (int y = yMin; y <= yMax; y++)
	                {
	                    int xpos = 0;
	                    for (int x = xMin; x <= xMax; x++)
	                    {
	                        Ecurve[xpos, ypos] = (float)Math.Pow((x0 - 2 * x + x2), 2.0) + (float)Math.Pow((y0 - 2 * y + y2), 2.0);

	                        if (Ecurve[xpos, ypos] > EcurveMax)
	                            EcurveMax = Ecurve[xpos, ypos];
	                        xpos++;
	                    }
	                    ypos++;
	                }

	                // Normalise Ecurve
	                for (int y = 0; y < Ecurve.GetLength(1); y++)
	                    for (int x = 0; x < Ecurve.GetLength(0); x++)
	                        Ecurve[x, y] /= EcurveMax;


	                // Eimage
	                float[,] Eimage = new float[xMax - xMin + 1, yMax - yMin + 1];
	                float EimageMin = float.MaxValue;
	                float EimageMax = float.MinValue;

	                ypos = 0;
	                for (int y = yMin; y <= yMax; y++)
	                {
	                    int xpos = 0;
	                    for (int x = xMin; x <= xMax; x++)
	                    {
	                        Eimage[xpos, ypos] = 255 - imageBuffer[y * bufferWidth + x];

	                        if (Eimage[xpos, ypos] > EimageMax)
	                            EimageMax = Eimage[xpos, ypos];

	                        if (Eimage[xpos, ypos] < EimageMin)
	                            EimageMin = Eimage[xpos, ypos];
	                        xpos++;
	                    }
	                    ypos++;
	                }

	                // Normalise Eimage
	                for (int y = 0; y < Eimage.GetLength(1); y++)
	                    for (int x = 0; x < Eimage.GetLength(0); x++)
	                        Eimage[x, y] = (Eimage[x, y] - EimageMin) / Math.Max(1, EimageMax - EimageMin);

	                // Choose final position for snake point
	                float EMin = float.MaxValue;
	                int EminX = 0, EminY = 0;
	                ypos = 0;
	                for (int y = yMin; y <= yMax; y++)
	                {
	                    int xpos = 0;
	                    for (int x = xMin; x <= xMax; x++)
	                    {
	                        float E = alpha * Econt[xpos, ypos] + beta * Ecurve[xpos, ypos] + gamma * Eimage[xpos, ypos];

	                        if (E < EMin)
	                        {
	                            bool neighbourConflict = false;
	                            foreach (SnakeNode neighour in this.neighbours)
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

	                if (EMin != float.MaxValue)
	                {
	                    this.x = EminX;
	                    this.y = EminY;
	                }
	            }
	            else
	            {
	                // Junction nodes

	                // Find nearest junctions to calculate Rmax
	                List<SnakeNode> junctionNeighbours = this.FindJunctionNeighbours();
	                double minimumJunctionDistance = double.MaxValue;

	                foreach (SnakeNode junctionNeighbour in junctionNeighbours)
	                {
	                    // Ignore boundary nodes
	                    if (junctionNeighbour.Neighbours.Count != 1)
	                    {
	                        double junctionDistance = Math.Sqrt(Math.Pow( this.X - junctionNeighbour.X, 2.0) + Math.Pow(this.y - junctionNeighbour.Y, 2.0));
	                        if (junctionDistance < minimumJunctionDistance)
	                            minimumJunctionDistance = junctionDistance;
	                    }
	                }

	                int Rmax = (int)(minimumJunctionDistance / 2.0);
	                int Rmin = 4;
	                int offsetX, offsetY;
	                byte[,] thresholdBuffer = FindThresholdedRegion(this.X, this.Y, imageBuffer, bufferWidth, bufferHeight, Rmax + m, true, out offsetX, out offsetY);

	                // Calculate bounds
	                int xMin = Math.Max(0, this.x - m);
	                int xMax = Math.Min(this.x + m, bufferWidth - 1);
	                int yMin = Math.Max(0, this.y - m);
	                int yMax = Math.Min(this.y + m, bufferHeight - 1);

	                // No Econt

	                // Ecurve
	                float[,] Ecurve = new float[xMax - xMin + 1, yMax - yMin + 1];
	                float EcurveMax = float.MinValue;

	                List<Point[]> neighbouringPoints = new List<Point[]>();

	                int neighbourIndex = 0;
	                foreach (SnakeNode neighbour in this.neighbours)
	                {
	                    neighbouringPoints.Add(new Point[2]);
	                    neighbouringPoints[neighbourIndex][0] = new Point(neighbour.X, neighbour.Y);
	                    if (neighbour.Neighbours.Count == 2)
	                    {
	                        // Find correct next neighbour
	                        if (neighbour.Neighbours[0] == this)
	                        {
	                            neighbouringPoints[neighbourIndex][1] = new Point(neighbour.neighbours[1].X, neighbour.neighbours[1].Y);
	                        }
	                        else
	                        {
	                            neighbouringPoints[neighbourIndex][1] = new Point(neighbour.neighbours[0].X, neighbour.neighbours[0].Y);
	                        }
	                    }
	                    else
	                    {
	                        // Current approach when n-2 doesn't exist is to not use that direction in the weight
	                        neighbouringPoints.RemoveAt(neighbouringPoints.Count - 1);
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
	                        Ecurve[xpos, ypos] = 0;
	                        foreach (Point[] currentPoints in neighbouringPoints)
	                        {
	                            Point p1 = currentPoints[0];
	                            Point p2 = currentPoints[1];
	                            Ecurve[xpos, ypos] += (float)((p0 - p1) - (p1 - p2)).LengthSquared;
	                        }
	                        if (Ecurve[xpos, ypos] > EcurveMax)
	                            EcurveMax = Ecurve[xpos, ypos];
	                        xpos++;
	                    }
	                    ypos++;
	                }

	                // Normalise Ecurve
	                for (int y = 0; y < Ecurve.GetLength(1); y++)
	                    for (int x = 0; x < Ecurve.GetLength(0); x++)
	                        Ecurve[x, y] /= EcurveMax;

	                // Eimage
	                float[,] Eimage = new float[xMax - xMin + 1, yMax - yMin + 1];
	                float EimageMin = float.MaxValue;
	                float EimageMax = float.MinValue;

	                ypos = 0;
	                for (int y = yMin; y <= yMax; y++)
	                {
	                    int xpos = 0;
	                    for (int x = xMin; x <= xMax; x++)
	                    {
	                        // Calculate Eimage based on concentric circles
	                        int xCentre = xMax - xMin;

	                        Eimage[xpos, ypos] = FindJunctionAppearanceWeight(x - offsetX, y - offsetY, thresholdBuffer, Rmax, Rmin);

	                        if (Eimage[xpos, ypos] > EimageMax)
	                            EimageMax = Eimage[xpos, ypos];

	                        if (Eimage[xpos, ypos] < EimageMin)
	                            EimageMin = Eimage[xpos, ypos];
	                        xpos++;
	                    }
	                    ypos++;
	                }

	                // Normalise Eimage
	                for (int y = 0; y < Eimage.GetLength(1); y++)
	                    for (int x = 0; x < Eimage.GetLength(0); x++)
	                        Eimage[x, y] = (Eimage[x, y] - EimageMin) / Math.Max(1, EimageMax - EimageMin);

	                // Choose final position for snake point
	                float EMin = float.MaxValue;
	                int EminX = 0, EminY = 0;
	                ypos = 0;
	                for (int y = yMin; y <= yMax; y++)
	                {
	                    int xpos = 0;
	                    for (int x = xMin; x <= xMax; x++)
	                    {
	                        // Energy formula
	                        float E = beta * Ecurve[xpos, ypos] + gamma * Eimage[xpos, ypos];

	                        if (E < EMin)
	                        {
	                            bool neighbourConflict = false;
	                            foreach (SnakeNode neighour in this.neighbours)
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

	                if (EMin != float.MaxValue)
	                {
	                    this.x = EminX;
	                    this.y = EminY;
	                }
	            }

	            Point finalPosition = new Point(this.x, this.y);
	            return startingPosition != finalPosition;
	        } */
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