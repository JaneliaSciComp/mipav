package gov.nih.mipav.model.algorithms.filters;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.util.*;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.*;
import java.awt.geom.*;

import WildMagic.LibFoundation.Mathematics.*;

public class AlgorithmProstateBoundaryFinding extends AlgorithmBase implements
		AlgorithmInterface {

	private VOI resultVOI;

	public ModelImage srcImage;

	private VOIVector VOIs;

	private Edge voiTable = new Edge();

	private Edge imageTable = new Edge();

	private Edge narrowBandTable = new Edge();

	private Hashtable<String, Integer> visited = new Hashtable<String, Integer>();

	private int narrowBandWidthUpper = 10;
	private int narrowBandWidthLower = 10;

	private Hashtable<String, Vector<Edge>> edgeRegionTable = new Hashtable<String, Vector<Edge>>();

	private Vector<Edge> edgeRefTable = new Vector<Edge>();

	private Vector<Edge> edgeRecoveryTable = new Vector<Edge>();

	private Hashtable<String, Vector<Edge>> filteredEdgeRegionTable = new Hashtable<String, Vector<Edge>>();

	private Hashtable<String, Vector<Edge>> finalEdgeRegionTable = new Hashtable<String, Vector<Edge>>();

	private Hashtable<String, Vector<Edge>> finalTable = new Hashtable<String, Vector<Edge>>();

	private Hashtable<String, Vector<Edge>> addEdgeTable = new Hashtable<String, Vector<Edge>>();

	private int scanDegree = 9;

	private int rotationAngleStart = 0;

	private int rotationAngleEnd = 360;

	public AlgorithmProstateBoundaryFinding(ModelImage _srcImage) {
		srcImage = _srcImage;
	}

	public void setNarrowBandWidth(int narrowBandWidth ) {
		narrowBandWidthUpper = narrowBandWidth;
		narrowBandWidthLower = narrowBandWidth;
	}
	
	public void algorithmPerformed(AlgorithmBase algorithm) {
		/*
		 * if (!algorithm.isCompleted()) { finalize(); return; }
		 */
	}

	
	 /**
     * Prepares this class for destruction.
     */
    public void finalize() {
    	
    	 voiTable.clear();
         voiTable = null;

         imageTable.clear();
         imageTable = null;

         narrowBandTable.clear();
         narrowBandTable = null;
    	
    	edgeRegionTable.clear();
    	edgeRegionTable = null;
        
    	edgeRefTable.clear();
    	edgeRefTable = null;
    	
    	edgeRecoveryTable.clear();
    	edgeRecoveryTable = null;
    	
    	filteredEdgeRegionTable.clear();
    	filteredEdgeRegionTable = null;
    	
    	finalEdgeRegionTable.clear();
    	finalEdgeRegionTable = null;
    	
    	finalTable.clear();
    	finalTable = null;
    	
    	addEdgeTable.clear();
    	addEdgeTable = null;
    	
    	visited.clear();
    	visited = null;
    	
        super.finalize();
    }

	
	/**
	 * Returns the resultant VOI.
	 * 
	 * @return resultant VOI that has localized to the boundaries of the object
	 */
	public VOI getResultVOI() {
		return resultVOI;
	}

	public void runAlgorithm() {
		createImagePointTable();
		createVOIPointTable();
		findNarrowBand();
		drawNarrowBand();
		
		traceContinuity(edgeRegionTable);
		// findLargestSpan();
		
		countContinuity(edgeRegionTable);
		
		
		createEdgeRefTable(edgeRegionTable);
		
		
		filterEdges();

		// sortEdges();
		// reomveUShapeEdges(filteredEdgeRegionTable);
		
	
		enhanceWeakLinkEdge(filteredEdgeRegionTable);
		
		// enhanceOutmostBoundary();
		findMaxEdges(filteredEdgeRegionTable, finalEdgeRegionTable);
		
		recoverEdges(finalEdgeRegionTable);
		rearrangeEdges(finalEdgeRegionTable);
		
		linkEdges();
		
		
		viewImage();
		viewAddEdge();
		
	    // fillPoints();
		
	     
	   
		// trimBranches();
		sortPoints();
		updateFinalEdge();
	    
	    
		// viewRefTable();

		setCompleted(true);
		System.gc();
	     
		// viewEdgeMap();
	}

	public void cleanMap() {
		int xDim = srcImage.getExtents()[0];
		int yDim = srcImage.getExtents()[1];
		visited.clear();
		for (int i = 0; i < yDim; i++) {
			for (int j = 0; j < xDim; j++) {
				visited.put(j + ":" + i, 0);
			}
		}
	}
	
	public boolean withinRange(int x, int y) {
		int xDim = srcImage.getExtents()[0];
		int yDim = srcImage.getExtents()[1];
		
		if ( x >= 0 && x <= xDim && y >= 0 && y <= yDim ) {
			return true;
		} else {
			return false;
		}
	}
	
	public void fillPoints() {
		int x, y;
		int xDim = srcImage.getExtents()[0];
		int yDim = srcImage.getExtents()[1];
        int degree;
        int value;
        
        cleanMap();
        
		Vector<Edge> edgeTable = new Vector<Edge>();
		
    	for (int i = 0; i < yDim; i++) {
			for (int j = 0; j < xDim; j++) {
				y = i;
				x = j;
				
				value = srcImage.get(x, y).intValue();
				
				if (value != 0) {
					
					Edge edge = new Edge();
					
					ArrayDeque<PolarPoint> q = new ArrayDeque<PolarPoint>();
					PolarPoint seed = new PolarPoint(x, y, 0, 0);
					q.offerLast(seed);
					
					while (!q.isEmpty()) {
						seed = q.getFirst();
						edge.insert(seed.x, seed.y, seed.r, seed.theta);
						q.removeFirst();
						// test the 8 neighbors
						x = seed.x;
						y = seed.y;
						
						// top left pixel
						if ( withinRange((x - 1), (y - 1))) {
							value = (Integer) visited.get((x - 1) + ":" + (y - 1));
							if (value == 0) {
								visited.put(((x - 1) + ":" + (y - 1)), 1);
								if (srcImage.get(x - 1, y - 1).intValue() != 0) {
									q.offerLast(new PolarPoint(x - 1, y - 1, 0, 0));
								}
							}
						}
						
						// top middle pixel
						if (withinRange(x , (y - 1))) {
							value = (Integer) visited.get(x + ":" + (y - 1));
							if (value == 0) {
								visited.put((x + ":" + (y - 1)), 1);
								if (srcImage.get(x, y - 1).intValue() != 0) {
									q.offerLast(new PolarPoint(x, y - 1, 0, 0));
								}
							}
						}
						
						// top right pixel
						if ( withinRange(x, (y - 1))) {
							value = (Integer) visited.get(x + ":" + (y - 1));
							if (value == 0) {
								visited.put((x + ":" + (y - 1)), 1);
								if (srcImage.get(x, y - 1).intValue() != 0) {
									q.offerLast(new PolarPoint(x, y - 1, 0, 0));
								}
							}
						}
						
						// top right pixel
						if (withinRange((x + 1) , (y - 1))) {
							value = (Integer) visited.get((x + 1) + ":" + (y - 1));
							if (value == 0) {
								visited.put(((x + 1) + ":" + (y - 1)), 1);
								if (srcImage.get(x + 1, y - 1).intValue() != 0) {
									q.offerLast(new PolarPoint(x + 1, y - 1, 0,
											0));
								}
							}
						}
						
						// left pixel
						if ( withinRange((x - 1) , y) ) {
							value = (Integer) visited.get((x - 1) + ":" + y);
								if (value == 0) {
									visited.put(((x - 1) + ":" + y), 1);
									if (srcImage.get(x - 1, y).intValue() != 0) {
										q.offerLast(new PolarPoint(x - 1, y, 0, 0));
									}
								}
						}
						
						// right pixel
						if ( withinRange((x + 1) , y) ) {
							value = (Integer) visited.get((x + 1) + ":" + y);
							if (value == 0) {
								visited.put(((x + 1) + ":" + y), 1);
								if (srcImage.get(x + 1, y).intValue() != 0) {
									q.offerLast(new PolarPoint(x + 1, y, 0, 0));
								}
							}
						}
						
						// lower left pixel
						if ( withinRange((x - 1), (y + 1)) ) {
							value = (Integer) visited.get((x - 1) + ":" + (y + 1));
							if (value == 0) {
								visited.put(((x - 1) + ":" + (y + 1)), 1);
								if (srcImage.get(x - 1, y + 1).intValue() != 0) {
									q.offerLast(new PolarPoint(x - 1, y + 1, 0, 0));
								}
							}
						}
						
						// lower middle pixel
						if ( withinRange(x, (y + 1)) ) {
							value = (Integer) visited.get(x + ":" + (y + 1));
							if (value == 0) {
								visited.put((x + ":" + (y + 1)), 1);
								if (srcImage.get(x, y + 1).intValue() != 0) {
									q.offerLast(new PolarPoint(x, y + 1, 0, 0));
								}
							}
						}
						
						// lower right pixel
						if ( withinRange((x + 1), (y + 1))) {
							value = (Integer) visited.get((x + 1) + ":" + (y + 1));
							if (value == 0) {
								visited.put(((x + 1) + ":" + (y + 1)), 1);
								if (srcImage.get(x + 1, y + 1).intValue() != 0) {
									q.offerLast(new PolarPoint(x + 1, y + 1, 0, 0));
								}
							}
						}

					}  // end while q
					// System.err.println("edge.size = " + edge.size());
					edgeTable.add(edge);
					
				}
			}
    	}
        
		for ( int i = 0; i < edgeTable.size(); i++ ) {
			Edge edge = edgeTable.get(i);
			
			Enumeration e = edge.getKeys();
			
			while ( e.hasMoreElements() ) {
				String key = (String) e.nextElement();
				String temp[] = key.split(":");

				x = Integer.valueOf(temp[0]);
				y = Integer.valueOf(temp[1]);

				degree = findDegree(x, y);
				
				// if ( degree <= 4 ) {
					
					PolarPoint p = findNearestPoint(x, y, edge, edgeTable);
					
					if ( p.x != -1 && p.y != -1 ) {
						linkPoints(x, y, p.x, p.y);
					}
					
				// }
				
				
			}
			
			
		}
		
		srcImage.notifyImageDisplayListeners(null, true);
		
	}

	public PolarPoint findNearestPoint(int x, int y, Edge edge, Vector<Edge> edgeTable ) {
		PolarPoint result = new PolarPoint();
		int xCurrent, yCurrent;
		int degree;
	    float minDistance = 999;
		float distance;
		int xTarget = -1, yTarget = -1;
		result.x = -1;
		result.y = -1;
		
		for ( int i = 0; i < edgeTable.size(); i++ ) {
			Edge currentEdge = edgeTable.get(i);
			
			// if ( currentEdge != edge ) {    // make sure not the current tracking edge
				Enumeration e = currentEdge.getKeys();
				
				while ( e.hasMoreElements() ) {
					String key = (String) e.nextElement();
					String temp[] = key.split(":");

					xCurrent = Integer.valueOf(temp[0]);
					yCurrent = Integer.valueOf(temp[1]);

					degree = findDegree(xCurrent, yCurrent);
					
					if ( degree <= 2) {
			              distance = (float)Math.sqrt((xCurrent-x)*(xCurrent-x) + (yCurrent-y)*(yCurrent-y));
			              if ( distance < minDistance ) {
			            	  minDistance = distance;
			            	  xTarget = xCurrent;
			            	  yTarget = yCurrent;
			              }
					}
					
				}
			// }
		}
		
		
		if ( xTarget != -1 && yTarget != -1) {
			result.x = xTarget;
			result.y = yTarget;
		}
		
		return result;
	}
	
	public void linkPoints(int srcX, int srcY, int destX, int destY) {
		
		    float x, y;
		
		    float distance = (float)Math.sqrt((destX-srcX)*(destX-srcX) + (destY-srcY)*(destY-srcY));
		    
		    if ( distance > 10 ) return;
		    
		    float currentDistance;
		    
		    float stepX = ( destX - srcX ) / 5f;
			float stepY = ( destY - srcY ) / 5f;
			
		    x = srcX;
		    y = srcY;
		    
		    currentDistance = (float)Math.sqrt((x-srcX)*(x-srcX) + (y-srcY)*(y-srcY));
		    
		    while (  currentDistance < distance ) {
		    	// System.err.println(" x = " + x + " y = " + y + " destX = " + destX + " destY = " + destY +  " stepX = " + stepX + " stepY = " + stepY);
		    	srcImage.set((int)x, (int)y, 250.0f);
		    	
		    	x += stepX;
		    	y += stepY;
		    	
		    	currentDistance = (float)Math.sqrt((x-srcX)*(x-srcX) + (y-srcY)*(y-srcY));
		    }
		
		
	}
	
	public int findDegree(int x, int y ) {
		int degree = 0;
		int upperLeft, upper, upperRight;
		int left, center, right;
		int lowerLeft, lower, lowerRight;
		
		upperLeft = srcImage.get(x - 1, y - 1).intValue();
		if (upperLeft != 0) {
			degree++;
		}

		upper = srcImage.get(x, y - 1).intValue();
		if (upper != 0) {
			degree++;
		}

		upperRight = srcImage.get(x + 1, y - 1).intValue();
		if (upperRight != 0) {
			degree++;
		}

		left = srcImage.get(x - 1, y).intValue();
		if (left != 0) {
			degree++;
		}

		right = srcImage.get(x + 1, y).intValue();
		if (right != 0) {
			degree++;
		}

		lowerLeft = srcImage.get(x - 1, y + 1).intValue();
		if (lowerLeft != 0) {
			degree++;
		}

		lower = srcImage.get(x, y + 1).intValue();
		if (lower != 0) {
			degree++;
		}

		lowerRight = srcImage.get(x + 1, y + 1).intValue();
		if (lowerRight != 0) {
			degree++;
		}

	    return degree;	
	}
	
	
	public void trimBranches() {
		int x, y;
		float r, theta;
		// PolarPoint seed = null;
		// PolarPoint newSeed = null;
		boolean found = false;
		int degree = 0;
		int num = 0;
		int size = 0;
		int upperLeft, upper, upperRight;
		int left, center, right;
		int lowerLeft, lower, lowerRight;

		int xDim = srcImage.getExtents()[0];
		int yDim = srcImage.getExtents()[1];

		for (int i = 0; i < yDim; i++) {
			for (int j = 0; j < xDim; j++) {
				y = i;
				x = j;

				center = srcImage.get(x, y).intValue();

				if (center != 0) {
					ArrayDeque<PolarPoint> q = new ArrayDeque<PolarPoint>();
					PolarPoint seed = new PolarPoint(x, y, 0, 0);
					q.offerLast(seed);

					while (!q.isEmpty()) {

						PolarPoint tempSeed = q.getFirst();
						q.removeFirst();

						x = tempSeed.x;
						y = tempSeed.y;

						degree = 0;

						upperLeft = srcImage.get(x - 1, y - 1).intValue();
						if (upperLeft != 0) {
							degree++;
						}

						upper = srcImage.get(x, y - 1).intValue();
						if (upper != 0) {
							degree++;
						}

						upperRight = srcImage.get(x + 1, y - 1).intValue();
						if (upperRight != 0) {
							degree++;
						}

						left = srcImage.get(x - 1, y).intValue();
						if (left != 0) {
							degree++;
						}

						right = srcImage.get(x + 1, y).intValue();
						if (right != 0) {
							degree++;
						}

						lowerLeft = srcImage.get(x - 1, y + 1).intValue();
						if (lowerLeft != 0) {
							degree++;
						}

						lower = srcImage.get(x, y + 1).intValue();
						if (lower != 0) {
							degree++;
						}

						lowerRight = srcImage.get(x + 1, y + 1).intValue();
						if (lowerRight != 0) {
							degree++;
						}

						if (degree == 0) {
							srcImage.set(x, y, 0);
						} else if (degree == 1) {
							PolarPoint newSeed = null;
							if (upperLeft != 0) {
								newSeed = new PolarPoint(x - 1, y - 1, 0, 0);
							} else if (upper != 0) {
								newSeed = new PolarPoint(x, y - 1, 0, 0);
							} else if (upperRight != 0) {
								newSeed = new PolarPoint(x + 1, y - 1, 0, 0);
							} else if (left != 0) {
								newSeed = new PolarPoint(x - 1, y, 0, 0);
							} else if (right != 0) {
								newSeed = new PolarPoint(x + 1, y, 0, 0);
							} else if (lowerRight != 0) {
								newSeed = new PolarPoint(x - 1, y + 1, 0, 0);
							} else if (lower != 0) {
								newSeed = new PolarPoint(x, y + 1, 0, 0);
							} else if (lowerRight != 0) {
								newSeed = new PolarPoint(x + 1, y + 1, 0, 0);
							}
							srcImage.set(x, y, 0);
							if (newSeed != null) {
								q.offerLast(newSeed);
							}
						} else if (degree == 2) {

							if (upperLeft != 0 && upper != 0) {
								srcImage.set(x, y, 0);
								PolarPoint newSeed = new PolarPoint(x - 1,
										y - 1, 0, 0);
								q.offerLast(newSeed);
								PolarPoint nextSeed = new PolarPoint(x, y - 1,
										0, 0);
								q.offerLast(nextSeed);
							} else if (upper != 0 && upperRight != 0) {
								srcImage.set(x, y, 0);
								PolarPoint newSeed = new PolarPoint(x, y - 1,
										0, 0);
								q.offerLast(newSeed);
								PolarPoint nextSeed = new PolarPoint(x + 1,
										y - 1, 0, 0);
								q.offerLast(nextSeed);
							} else if (upperRight != 0 && right != 0) {
								srcImage.set(x, y, 0);
								PolarPoint newSeed = new PolarPoint(x + 1,
										y - 1, 0, 0);
								q.offerLast(newSeed);
								PolarPoint nextSeed = new PolarPoint(x + 1, y,
										0, 0);
								q.offerLast(nextSeed);
							} else if (right != 0 && lowerRight != 0) {
								srcImage.set(x, y, 0);
								PolarPoint newSeed = new PolarPoint(x + 1, y,
										0, 0);
								q.offerLast(newSeed);
								PolarPoint nextSeed = new PolarPoint(x + 1,
										y + 1, 0, 0);
								q.offerLast(nextSeed);
							} else if (lowerRight != 0 && lower != 0) {
								srcImage.set(x, y, 0);
								PolarPoint newSeed = new PolarPoint(x + 1,
										y + 1, 0, 0);
								q.offerLast(newSeed);
								PolarPoint nextSeed = new PolarPoint(x, y + 1,
										0, 0);
								q.offerLast(nextSeed);
							} else if (lower != 0 && lowerLeft != 0) {
								srcImage.set(x, y, 0);
								PolarPoint newSeed = new PolarPoint(x, y + 1,
										0, 0);
								q.offerLast(newSeed);
								PolarPoint nextSeed = new PolarPoint(x - 1,
										y + 1, 0, 0);
								q.offerLast(nextSeed);
							} else if (lowerLeft != 0 && left != 0) {
								srcImage.set(x, y, 0);
								PolarPoint newSeed = new PolarPoint(x - 1,
										y + 1, 0, 0);
								q.offerLast(newSeed);
								PolarPoint nextSeed = new PolarPoint(x - 1, y,
										0, 0);
								q.offerLast(nextSeed);
							} else if (left != 0 && upperLeft != 0) {
								srcImage.set(x, y, 0);
								PolarPoint newSeed = new PolarPoint(x - 1, y,
										0, 0);
								q.offerLast(newSeed);
								PolarPoint nextSeed = new PolarPoint(x - 1,
										y - 1, 0, 0);
								q.offerLast(nextSeed);
							}
						} else {
							if (left != 0 && upperLeft != 0 && upper != 0) {
								srcImage.set(x - 1, y - 1, 0);
							}
							if (upper != 0 && upperRight != 0 && right != 0) {
								srcImage.set(x + 1, y - 1, 0);
							}

							if (right != 0 && lowerRight != 0 && lower != 0) {
								srcImage.set(x + 1, y + 1, 0);
							}

							if (lower != 0 && lowerLeft != 0 && left != 0) {
								srcImage.set(x - 1, y + 1, 0);
							}
						}
						
					} // end while q !isEmpty
				} // end if center != 0
			}
		}

		srcImage.notifyImageDisplayListeners(null, true);
	}

	public void sortPoints() {
		int x, y;
		float r, theta;

		Vector<Vector3f> result = new Vector<Vector3f>();

		for (int i = rotationAngleStart; i < rotationAngleEnd; i += scanDegree) {

			Vector<Edge> addTable = addEdgeTable
					.get((i + ":" + (i + scanDegree)));
			Vector<Edge> edgeTable = finalEdgeRegionTable
					.get((i + ":" + (i + scanDegree)));

			Vector<Edge> newTable = new Vector<Edge>();

			Vector<PolarPoint> pointVector = new Vector<PolarPoint>();

			Edge edge;
			Edge add;

			Edge newEdge = new Edge();

			int intensity;

			if (edgeTable != null) {
				edge = (Edge) edgeTable.get(0);
				if (edge != null) {

					Enumeration e = edge.getKeys();
					while (e.hasMoreElements()) {

						String key = (String) e.nextElement();
						String temp[] = key.split(":");

						x = Integer.valueOf(temp[0]);
						y = Integer.valueOf(temp[1]);

						String value = edge.find(x, y);
						String[] values = value.split(":");

						r = Float.valueOf(values[0]);
						theta = Float.valueOf(values[1]);
						// newEdge.insert(x, y, r, theta);

						intensity = srcImage.get(x, y).intValue();

						if (intensity != 0) {
							pointVector.add(new PolarPoint(x, y, r, theta));
						}
					}

				}
			}

			if (addTable != null) {
				add = (Edge) addTable.get(0);
				if (add != null) {

					Enumeration e = add.getKeys();
					while (e.hasMoreElements()) {

						String key = (String) e.nextElement();
						String temp[] = key.split(":");

						x = Integer.valueOf(temp[0]);
						y = Integer.valueOf(temp[1]);

						String value = add.find(x, y);
						String[] values = value.split(":");

						r = Float.valueOf(values[0]);
						theta = Float.valueOf(values[1]);
						// newEdge.insert(x, y, r, theta);

						intensity = srcImage.get(x, y).intValue();

						if (intensity != 0) {
							pointVector.add(new PolarPoint(x, y, r, theta));
						}
					}
				}
			}

			int size = pointVector.size();
			PolarPoint[] points = new PolarPoint[size];
			for (int k = 0; k < size; k++) {
				points[k] = new PolarPoint();
				points[k].assign(pointVector.elementAt(k));
			}

			PolarPoint p = new PolarPoint();
			QuickSort qsort = new QuickSort(p);
			qsort.sort(points);

			PolarPoint pPrev, pNext;
			for (int j = 0; j < size; j++) {
				if (j % 6 == 0) {
					
					p = points[j];
					
					if ( !isInteriorPoint(p, points, j)) {
						result.add(new Vector3f(p.x, p.y, 0f));
						newEdge.insert(p.x, p.y, 0, 0);
					}
				}
			}

			newTable.add(newEdge);
			finalTable.put((i + ":" + (i + scanDegree)), newTable);
		}

		int len = result.size();

		Vector3f[] voiList = new Vector3f[len];

		for (int i = 0; i < len; i++) {
			voiList[i] = result.get(i);
		}

		resultVOI = new VOI((short) srcImage.getVOIs().size(), "prostate",
				VOI.CONTOUR, -1.0f);
		resultVOI.importCurve(voiList);

	}

	public boolean isInteriorPoint(PolarPoint p, PolarPoint[] points, int index) {
		boolean isInteriorPoint = false;
		
		PolarPoint currentPoint;
		float theta_p = p.theta;
		float theta;
		float avg_r = 0; 
		int count = 0;
		
		for ( int i = 0; i < points.length; i++ ) {
			if ( points[i] != null ) {
				currentPoint = points[i];
			    theta = currentPoint.theta;	
				
			    if( theta >= (theta_p - 3) && theta <= (theta_p+3) ) { 
			    	
			    		avg_r += currentPoint.r;
			    		count++;
			    }
				
			}
		}
		
		avg_r /= count;
		if ( p.r <= (avg_r - 1) ) isInteriorPoint = true;
		
		return isInteriorPoint;
	}
	
	public void collectPoints() {

		int x, y;
		float r, theta;

		for (int i = rotationAngleStart; i < rotationAngleEnd; i += scanDegree) {

			Vector<Edge> addTable = addEdgeTable
					.get((i + ":" + (i + scanDegree)));
			Vector<Edge> edgeTable = finalEdgeRegionTable
					.get((i + ":" + (i + scanDegree)));

			Vector<Edge> newTable = new Vector<Edge>();

			Edge edge;
			Edge add;

			Edge newEdge = new Edge();

			if (edgeTable != null) {

				edge = (Edge) edgeTable.get(0);

				if (edge != null) {
					Enumeration e = edge.getKeys();
					while (e.hasMoreElements()) {

						String key = (String) e.nextElement();
						String temp[] = key.split(":");

						x = Integer.valueOf(temp[0]);
						y = Integer.valueOf(temp[1]);

						String value = edge.find(x, y);
						String[] values = value.split(":");

						r = Float.valueOf(values[0]);
						theta = Float.valueOf(values[1]);
						newEdge.insert(x, y, r, theta);
						break;

					}

				}
			} else if (addTable != null) {

				add = (Edge) addTable.get(0);

				Enumeration e = add.getKeys();

				while (e.hasMoreElements()) {

					String key = (String) e.nextElement();
					String temp[] = key.split(":");

					x = Integer.valueOf(temp[0]);
					y = Integer.valueOf(temp[1]);

					String value = add.find(x, y);
					String[] values = value.split(":");

					r = Float.valueOf(values[0]);
					theta = Float.valueOf(values[1]);
					newEdge.insert(x, y, r, theta);
					break;

				}

			}

			newTable.add(newEdge);
			finalTable.put((i + ":" + (i + scanDegree)), newTable);
		}

	}

	public void rearrangeEdges(Hashtable<String, Vector<Edge>> fEdgeTable) {

		fEdgeTable.clear();

		int x, y;
		int k;
		float r, theta;
		Vector<Edge> edgesInRegion = new Vector<Edge>();

		for (int i = rotationAngleStart; i < rotationAngleEnd; i += scanDegree) {

			Vector<Edge> edgeTable = new Vector<Edge>();

			Edge newEdge = new Edge();

			edgesInRegion.clear();

			for (k = 0; k < edgeRecoveryTable.size(); k++) {
				Edge edge = edgeRecoveryTable.get(k);

				Enumeration e = edge.getKeys();
				while (e.hasMoreElements()) {

					String key = (String) e.nextElement();
					String temp[] = key.split(":");

					x = Integer.valueOf(temp[0]);
					y = Integer.valueOf(temp[1]);

					String value = edge.find(x, y);
					String[] values = value.split(":");

					r = Float.valueOf(values[0]);
					theta = Float.valueOf(values[1]);

					if (theta >= i && theta <= (i + scanDegree)) {
						edgesInRegion.add(edge);
						break;
					}

				}

			}

			// copy to new edge
			if (edgesInRegion.size() > 0) {
				Edge best = edgesInRegion.get(0);
				float maxWeight = 0;
				for (k = 0; k < edgesInRegion.size(); k++) {
					Edge edge = edgesInRegion.get(k);
					if (edge.weight > maxWeight) {
						maxWeight = edge.weight;
						best = edge;
					}
				}

				Enumeration e = best.getKeys();
				while (e.hasMoreElements()) {

					String key = (String) e.nextElement();
					String temp[] = key.split(":");

					x = Integer.valueOf(temp[0]);
					y = Integer.valueOf(temp[1]);

					String value = best.find(x, y);
					String[] values = value.split(":");

					r = Float.valueOf(values[0]);
					theta = Float.valueOf(values[1]);

					if (theta >= i && theta <= (i + scanDegree)) {
						newEdge.insert(x, y, r, theta);
					}
				}

				edgeTable.add(newEdge);

				fEdgeTable.put((i + ":" + (i + scanDegree)), edgeTable);
			}
		}

	}

	public void recoverEdges(Hashtable<String, Vector<Edge>> fEdgeTable) {

		Edge edge;
		int x, y;
		float r, theta;
		boolean found = false;

		for (int i = rotationAngleStart; i < rotationAngleEnd; i += scanDegree) {

			Edge ref = null;

			Vector<Edge> edgeTable;

			edgeTable = fEdgeTable.get((i + ":" + (i + scanDegree)));

			if (edgeTable == null)
				continue;

			edge = edgeTable.get(0);

			Enumeration e = edge.getKeys();

			while (e.hasMoreElements()) {

				String key = (String) e.nextElement();
				String temp[] = key.split(":");

				x = Integer.valueOf(temp[0]);
				y = Integer.valueOf(temp[1]);

				found = false;

				for (int k = 0; k < edgeRefTable.size(); k++) {
					ref = edgeRefTable.get(k);

					if (ref.find(x, y) != null && ref.visited == false) {
						found = true;
						edgeRecoveryTable.add(ref);
						ref.visited = true;
						break;
					}
				}

				if (found == true)
					break;

			}

		}

	}

	public void findMaxEdges(Hashtable<String, Vector<Edge>> edgeRegTable,
			Hashtable<String, Vector<Edge>> fEdgeTable) {

		Edge edge;
		// Edge edgeMaxWeight = null;
		int x, y;
		float r, theta;

		fEdgeTable.clear();

		// collect the maximum weighted edge, and put them into the final edge
		// table.
		for (int i = rotationAngleStart; i < rotationAngleEnd; i += scanDegree) {

			Vector<Edge> edgeTable;

			edgeTable = edgeRegTable.get((i + ":" + (i + scanDegree)));

			Vector<Edge> filteredEdgeTable = new Vector<Edge>();

			float maxWeight = 0f;

			Edge edgeMaxWeight = null; // max weight edge should be null. Need to
									// deal with this case. !!!!!!!!!!!!

			for (int ii = 0; ii < edgeTable.size(); ii++) {

				edge = (Edge) edgeTable.get(ii);

				if (maxWeight < edge.weight) {
					maxWeight = edge.weight;
					// edgeMaxWeight = edge;
					edgeMaxWeight = new Edge();

					// copy each point from edge to edgeMaxWeight
					Enumeration e = edge.getKeys();

					while (e.hasMoreElements()) {

						String key = (String) e.nextElement();
						String temp[] = key.split(":");

						x = Integer.valueOf(temp[0]);
						y = Integer.valueOf(temp[1]);

						String value = edge.find(x, y);
						String[] values = value.split(":");

						r = Float.valueOf(values[0]);
						theta = Float.valueOf(values[1]);

						edgeMaxWeight.insert(x, y, r, theta);

					}

				}

			}

			if (edgeMaxWeight != null) {
				filteredEdgeTable.add(edgeMaxWeight);
				fEdgeTable.put((i + ":" + (i + scanDegree)), filteredEdgeTable);
			} else {
				System.err.println("max edge:  i = " + i + " (i+scanDegree) = " + ( i+scanDegree));
				// System.err.println("ax = " +  edgeTable.size());
				// Vector<Edge> edgeTbl = edgeRegionTable.get((i + ":" + (i + scanDegree)));
				// filteredEdgeTable.add(edgeTbl.get(0));
				// fEdgeTable.put((i + ":" + (i + scanDegree)), filteredEdgeTable);
			}
		}  // end for i = rotationAngle

	}

	public void linkEdges() {

		// retrace the continuity with the final edge region table
		traceContinuity(finalEdgeRegionTable);

		// link disconnected edges
		Vector<Edge> edgeTableCurrent;

		for (int i = rotationAngleStart; i < rotationAngleEnd; i += scanDegree) {

			edgeTableCurrent = finalEdgeRegionTable.get((i + ":" + (i + scanDegree)));

			if (edgeTableCurrent == null) { // deal with null case.
											// !!!!!!!!!!!!!!!!
				// System.err.println("i = " + i + " (i+scanDegree) = " +
				// (i+scanDegree));
				continue;
			}
			Edge edgeCurrent = null;

			edgeCurrent = (Edge) edgeTableCurrent.get(0);

			// if (edgeCurrent.isNew == true) continue;

			int degree = i;
			// search for the next edge connection

			
			while (edgeCurrent != null) {
				if (edgeCurrent.next != null) {
					i += scanDegree;
					degree += scanDegree;
					edgeCurrent = edgeCurrent.next;
				} else { // edgeCurrent.next == null;
					break;
				}
			}

			addEdge(edgeCurrent, (degree + scanDegree));
	
		}

	}

	public void addEdge(Edge edgeCurrent, int startDegree) {
		String key;
		String[] keys;
		String value;
		String[] values;
		int x, y;
		float r, theta;
		int count = 0;
		float rCurrent = 0f;
		float xCurrent = 0f;
		float yCurrent = 0f;
		float rCurrentAvg = 0f;
		float thetaCurrent = 0f;
		float degreeCurrent = 0;

		float rNext = 0f;
		float xNext = 0f;
		float yNext = 0f;
		float thetaNext = 0f;
		float rNextAvg = 0f;

		Edge edge;
		Edge edgeBest = null;
		float xBest = 0;
		float yBest = 0;
		float thetaBest = 0;
		float rBest = 0;
		float degreeBest = 0;

		float rNearestBest = 0;
		float rAvgBest = 999;

		Vector<Edge> edgeTableNext;

		float maxTheta = 0, minTheta = 999;
		float maxR = 0, minR = 0;
		// should not use the average. Average is wrong.
		// The radius should be the nearest point (r) between the two edges.
		// how to find the nearest point???

		Enumeration e = edgeCurrent.getKeys();
		while (e.hasMoreElements()) {

			key = (String) e.nextElement();
			keys = key.split(":");

			x = Integer.valueOf(keys[0]);
			y = Integer.valueOf(keys[1]);

			value = edgeCurrent.find(x, y);
			values = value.split(":");

			r = Float.valueOf(values[0]);
			theta = Float.valueOf(values[1]);

			if (r > maxR) {
				// maxTheta = theta;
				maxR = r;
				xCurrent = x;
				yCurrent = y;
				rCurrent = r;
				thetaCurrent = theta;
			}

			rCurrentAvg += r;
			count++;
		}

		rCurrentAvg /= count;
		edgeCurrent.rNearest = rCurrent;
		edgeCurrent.rAvg = rCurrentAvg;
		degreeCurrent = thetaCurrent;

		// look ahead for four blocks.
		for (int ii = 0; ii < 4; ii++) {

		    // if ( startDegree > 360 ) startDegree -= 360;

			edgeTableNext = finalEdgeRegionTable.get((startDegree + ":" + (startDegree + scanDegree)));

			if (edgeTableNext == null) {
				// System.err.println("startDegree = " + startDegree + " (startDegree+scanDegree) = " + (startDegree + scanDegree));
				startDegree += scanDegree;
				continue;
			}

			edge = (Edge) edgeTableNext.get(0);

			e = edge.getKeys();

			minTheta = 999;
			maxR = 0;
			rNext = 0;
			rNextAvg = 0;
			count = 0;
			// calculate the average
			while (e.hasMoreElements()) {

				key = (String) e.nextElement();
				keys = key.split(":");

				x = Integer.valueOf(keys[0]);
				y = Integer.valueOf(keys[1]);

				value = edge.find(x, y);
				values = value.split(":");

				r = Float.valueOf(values[0]);
				theta = Float.valueOf(values[1]);

				if (r > maxR) {
					// minTheta = theta;
					maxR = r;
					rNext = r;
					xNext = x;
					yNext = y;
					thetaNext = theta;
				}

				rNextAvg += r;
				count++;
			}
			rNextAvg /= count;

			// how to find the best edge, nearest radius, or outmost edge?
			// which table the edge should be added.
			// now just for the nearest radius, give a try.
			edge.rNearest = rNext;
			edge.rAvg = rNextAvg;
			float dist = 0;

			// dist = Math.abs(edge.rNearest - edgeCurrent.rNearest);
			// dist = (float)Math.sqrt((yNext - yCurrent)*(yNext -
			// yCurrent)+(xNext-xCurrent)*(xNext-xCurrent));
			if (rNext > rNearestBest) {
				rNearestBest = edge.rNearest;
				// edgeBest = edge;
				xBest = xNext;
				yBest = yNext;
				rBest = rNext;
				thetaBest = thetaNext;
				degreeBest = thetaNext;
			}

			startDegree += scanDegree;
		} // end for ii

		/*
		 * if ( degreeCurrent < degreeBest ) { linkTwoEdges(degreeCurrent,
		 * degreeBest, rCurrent, rBest); }
		 */
		
		
		float distance  = (float)Math.sqrt((xBest-xCurrent)*(xBest-xCurrent) + (yBest-yCurrent)*(yBest-yCurrent));
		
	   /*
		if (degreeBest > 360 && degreeCurrent < 360) { // cross 360.
			float rstep = (rBest - rCurrent) / distance;
			float rMidValue = rCurrent + rstep * (360 - degreeCurrent);
			
			Vector2f in = new Vector2f(rMidValue, 360);
			Vector2f out = new Vector2f(0, 0);

			MipavCoordinateSystems.PolarToCartesian2D(in, out, srcImage);

			x = (int) out.X;
			y = (int) out.Y;

			
			float distance1  = (float)Math.sqrt((x-xCurrent)*(x-xCurrent) + (y-yCurrent)*(y-yCurrent));
			float distance2  = (float)Math.sqrt((xBest-x)*(xBest-x) + (yBest-y)*(yBest-y));	
			linkTwoEdges(degreeCurrent, 360, rCurrent, rMidValue, distance1);
			linkTwoEdges(0, degreeBest, rMidValue, rBest, distance2);
		} else {
			linkTwoEdges(degreeCurrent, degreeBest, rCurrent, rBest, distance);
		}
		*/
		linkTwoEdges(degreeCurrent, degreeBest, rCurrent, rBest, distance);
         
	}

	public void linkTwoEdges(float degreeCurrent, float degreeBest,float rCurrent, float rBest, float distance) {
		// link the edgeBest and edgeCurrent.
		int start;
		int end;
		int x, y;
		float r;
		float theta = -1;

		
		// System.err.println("degreeCurrent = " + degreeCurrent + "  degreeBest = " + degreeBest  + " rCurrent = " + rCurrent + " rBest = " + rBest);
		
		start = findRegionStartDegree(degreeCurrent);
		end = findRegionEndDegree(degreeBest);
		// System.err.println("start = " + start + "  end = " + end);
		
		if (degreeCurrent == degreeBest) {
			// System.err.println("degreeCurrent = " + degreeCurrent);
			return;
		}

		float rStep = (rBest - rCurrent) / (distance);
		float thetaStep = (degreeBest - degreeCurrent) / (distance);

		Edge newEdge = null;

		float startPointInRegion;
		float endPointInRegion;

		r = rCurrent;

		boolean reachEnd = false;

		boolean first = false; 
		
		for (int i = start; i < end; i += scanDegree) {

			Vector<Edge> filteredEdgeTable = new Vector<Edge>();

			newEdge = new Edge();

			if (degreeCurrent >= i && degreeCurrent <= (i + scanDegree)) {
				startPointInRegion = degreeCurrent;
			} else {
				startPointInRegion = i;
			}

			if (degreeBest >= i && degreeBest <= (i + scanDegree)) {
				endPointInRegion = degreeBest;
				reachEnd = true;
			} else {
				endPointInRegion = i + scanDegree;
			}
		
			if ( first == false ) {
				System.err.println("begin:   " +  "  r = " + r + " theta = " + startPointInRegion);
				first = true;
			}
		
			// pause();
			// System.err.println("i = "+ i + " i+scanDegree = " +
			// (i+scanDegree));
			// System.err.println("startPointInRegion = " + startPointInRegion +
			// " endPointInRegion = " + endPointInRegion);
			for (float j = startPointInRegion; j <= endPointInRegion; j = j + thetaStep) {

				theta = j;

				// dangerous
				Vector2f in = new Vector2f(r, theta);
				Vector2f out = new Vector2f(0, 0);

				MipavCoordinateSystems.PolarToCartesian2D(in, out, srcImage);

				x = (int) out.X;
				y = (int) out.Y;

				// System.err.println("x = " + x + " y = " + y + " r = " + r + "
				// theta = " + theta);

				newEdge.insert(x, y, r, theta);

				r += rStep;
			}

			
			
			filteredEdgeTable.add(newEdge);
			addEdgeTable.put(i + ":" + (i + scanDegree), filteredEdgeTable);

			if (reachEnd == true) {
				break;
			}

		}
		
		System.err.println("end:   " +  "  r = " + r + " theta = " + theta);
		
	}

	public int findRegionStartDegree(float degree) {
		for (int i = rotationAngleStart; i < rotationAngleEnd; i += scanDegree) {
			if (degree >= i && degree <= (i + scanDegree)) {
				return i;
			}
		}
		return -1;
	}

	public int findRegionEndDegree(float degree) {
		for (int i = rotationAngleStart; i < rotationAngleEnd; i += scanDegree) {
			if (degree >= i && degree <= (i + scanDegree)) {
				return (i + scanDegree);
			}
		}
		return -1;
	}

	public void filterEdges() {

		for (int i = rotationAngleStart; i < rotationAngleEnd; i += scanDegree) {

			Vector<Edge> edgeTable = edgeRegionTable.get((i + ":" + (i + scanDegree)));

			
			Vector<Edge> filteredEdgeTable = new Vector<Edge>();

			Edge edge;

			for (int ii = 0; ii < edgeTable.size(); ii++) {

				edge = (Edge) edgeTable.get(ii);

				if (edge.weight >= 0.1f) {

					filteredEdgeTable.add(edge);

				}
			}

			filteredEdgeRegionTable.put((i + ":" + (i + scanDegree)), filteredEdgeTable);

		}
	}

	public void findNarrowBand() {

		// walk along each VOI contour point
		Vector<PolarPoint> voiPointsInRange = new Vector<PolarPoint>();

		for (int d = rotationAngleStart; d < rotationAngleEnd; d += scanDegree) {

			voiPointsInRange.clear();

			findVOIpointsInRange(voiPointsInRange, d, d + scanDegree);

			Vector<Edge> edgeTable = new Vector<Edge>();

			findRegion(d, d + scanDegree, voiPointsInRange);

			createEdgeTable(edgeTable);

			// traceOrientation(edgeTable, voiPointsInRange);

			edgeRegionTable.put((d + ":" + (d + scanDegree)), edgeTable);

			narrowBandTable.clear();
		}

	}

	
	 public void drawNarrowBand() {
			
			ModelImage tempImage = (ModelImage)srcImage.clone();
			tempImage.makeImageName("ruida", ".xml");
			int[] ext = tempImage.getExtents();
			int size = ext[0] * ext[1];
			float[] b = new float[size];
			
			// testImage.exportData(0, size, b);
			
			for ( int i = 0; i < size; i++ ) {
				int x = (int)(i % ext[0]);
				int y = (int)(i / ext[1]);
				if ( inRange(x, y) ) {
				   b[i] = 1;
				} else {
					b[i] = 0;
				}
				
			}
			try {
				tempImage.importData(0, b, true);
				System.err.println("ruida view image");
			  new ViewJFrameImage(tempImage);
			} catch ( Exception e ) {
				e.printStackTrace();
			}
	    } 
	
	
	public void createEdgeRefTable(Hashtable<String, Vector<Edge>> edgeTable) {
		Vector<Edge> edgeTableCurrent;
		int x, y;
		float r, theta;
		float weight;

		for (int i = rotationAngleStart; i < rotationAngleEnd; i += scanDegree) {

			edgeTableCurrent = edgeTable.get((i + ":" + (i + scanDegree)));

			Edge edgeCurrent;

			for (int ii = 0; ii < edgeTableCurrent.size(); ii++) {
				
				edgeCurrent = (Edge) edgeTableCurrent.get(ii);

				if (edgeCurrent.visited == true)
					continue; // skip edge scan

				Edge newEdge = new Edge();
				weight = 0;

				// insert the current edge points.
				if (edgeCurrent != null) {

					weight += edgeCurrent.weight;

					Enumeration e = edgeCurrent.getKeys();

					while (e.hasMoreElements()) {
						String key = (String) e.nextElement();
						String temp[] = key.split(":");

						x = Integer.valueOf(temp[0]);
						y = Integer.valueOf(temp[1]);

						String value = edgeCurrent.find(x, y);
						String[] values = value.split(":");

						r = Float.valueOf(values[0]);
						theta = Float.valueOf(values[1]);

						newEdge.insert(x, y, r, theta);

					}
				}

				// insert for the prev edge connections
				edgeCurrent = (Edge) edgeTableCurrent.get(ii);
				while (edgeCurrent != null && edgeCurrent.prev != null) {

					edgeCurrent = edgeCurrent.prev;

					weight += edgeCurrent.weight;

					edgeCurrent.visited = true;

					Enumeration e = edgeCurrent.getKeys();

					while (e.hasMoreElements()) {
						String key = (String) e.nextElement();
						String temp[] = key.split(":");

						x = Integer.valueOf(temp[0]);
						y = Integer.valueOf(temp[1]);

						String value = edgeCurrent.find(x, y);
						String[] values = value.split(":");

						r = Float.valueOf(values[0]);
						theta = Float.valueOf(values[1]);

						newEdge.insert(x, y, r, theta);

					}

				}

				// insert the next edge connections.
				edgeCurrent = (Edge) edgeTableCurrent.get(ii);
				while (edgeCurrent != null && edgeCurrent.next != null) {

					edgeCurrent = edgeCurrent.next;

					weight += edgeCurrent.weight;

					edgeCurrent.visited = true;

					Enumeration e = edgeCurrent.getKeys();

					while (e.hasMoreElements()) {
						String key = (String) e.nextElement();
						String temp[] = key.split(":");

						x = Integer.valueOf(temp[0]);
						y = Integer.valueOf(temp[1]);

						String value = edgeCurrent.find(x, y);
						String[] values = value.split(":");

						r = Float.valueOf(values[0]);
						theta = Float.valueOf(values[1]);

						newEdge.insert(x, y, r, theta);

					}

				}

				newEdge.weight = weight;
				edgeRefTable.add(newEdge);
				edgeCurrent.visited = true;

			}
		}
	}

	public void viewRefTable() {
		int position;

		int xDim = srcImage.getExtents()[0];
		int yDim = srcImage.getExtents()[1];
		int sliceSize = xDim * yDim;
		double[] sourceBuffer = new double[sliceSize];
		double[] originalBuffer = new double[sliceSize];

		try {

			srcImage.exportData(0, sliceSize, sourceBuffer);
			srcImage.exportData(0, sliceSize, originalBuffer);

			for (int y = 0; y < yDim; y++) {
				for (int x = 0; x < xDim; x++) {
					position = x + y * xDim;
					sourceBuffer[position] = 0;
				}
			}

			int x, y;
			float r1, r2, theta1, theta2;

			float min = 999, max = -999;
			float diff = 0;

			for (int i = 0; i < edgeRecoveryTable.size(); i++) {
				/*
				 * for (y = 0; y < yDim; y++) { for (x = 0; x < xDim; x++) {
				 * position = x + y * xDim; sourceBuffer[position] = 0; } }
				 */
				Edge edge = edgeRecoveryTable.get(i);

				Enumeration e = edge.getKeys();

				while (e.hasMoreElements()) {

					String key = (String) e.nextElement();
					String temp[] = key.split(":");

					x = Integer.valueOf(temp[0]);
					y = Integer.valueOf(temp[1]);

					position = x + y * xDim;
					sourceBuffer[position] = 250;
				}

				// pause();
				srcImage.importData(0, sourceBuffer, true);
				srcImage.calcMinMax();
				srcImage.notifyImageDisplayListeners(null, true);

			}

		} catch (FileNotFoundException err) {
			err.printStackTrace();
		} catch (IOException err) {
			err.printStackTrace();
		}

	}

	public void countContinuity(Hashtable<String, Vector<Edge>> edgeTable) {

		Vector<Edge> edgeTableCurrent;

		for (int i = rotationAngleStart; i < rotationAngleEnd; i += scanDegree) {

			edgeTableCurrent = edgeTable.get((i + ":" + (i + scanDegree)));

			Edge edgeCurrent;

			for (int ii = 0; ii < edgeTableCurrent.size(); ii++) {
				edgeCurrent = (Edge) edgeTableCurrent.get(ii);

				int count = 0;

				// search for the prev edge connection

				while (edgeCurrent != null && edgeCurrent.prev != null) {
					count++;
					edgeCurrent = edgeCurrent.prev;
				}

				edgeCurrent = (Edge) edgeTableCurrent.get(ii);

				while (edgeCurrent != null && edgeCurrent.next != null) {
					count++;
					edgeCurrent = edgeCurrent.next;
				}

				edgeCurrent = (Edge) edgeTableCurrent.get(ii);
				edgeCurrent.weight += (0.1f * count);

				// System.err.println("edgeCurrent.weight = " +
				// edgeCurrent.weight);

			}
		}
	}

	public void reomveUShapeEdges(Hashtable<String, Vector<Edge>> edgeTable) {
		Vector<Edge> edgeTableCurrent;
		Vector<Edge> currentEdges = new Vector<Edge>();
		Edge edgeList = new Edge();
		String key;
		int x, y;
		String[] keys;
		int degree = 0;
		boolean first = false;
		float startX = 0, startY = 0;
		float endX = 0, endY = 0;
		float midX = 0, midY = 0;
		float currentX = 0, currentY = 0;
		int endCount = 0;

		for (int i = rotationAngleStart; i < rotationAngleEnd; i += scanDegree) {

			edgeTableCurrent = edgeTable.get((i + ":" + (i + scanDegree)));

			Edge edge;

			// collect the current linked edges
			for (int ii = 0; ii < edgeTableCurrent.size(); ii++) {
				currentEdges.clear();
				edgeList.clear();
				edge = (Edge) edgeTableCurrent.get(ii);

				Enumeration e = edge.getKeys();

				while (e.hasMoreElements()) {

					key = (String) e.nextElement();
					keys = key.split(":");

					x = Integer.valueOf(keys[0]);
					y = Integer.valueOf(keys[1]);

					edgeList.insert(x, y, 0, 0);

				}

				currentEdges.add(edge);

			
				while (edge.next != null) {
					currentEdges.add(edge.next);
					edge = edge.next;

					e = edge.getKeys();

					while (e.hasMoreElements()) {

						key = (String) e.nextElement();
						keys = key.split(":");

						x = Integer.valueOf(keys[0]);
						y = Integer.valueOf(keys[1]);

						edgeList.insert(x, y, 0, 0);

					}
				}

				edge = (Edge) edgeTableCurrent.get(ii);

				while (edge.prev != null) {
					currentEdges.add(edge.prev);
					edge = edge.prev;

					e = edge.getKeys();

					while (e.hasMoreElements()) {

						key = (String) e.nextElement();
						keys = key.split(":");

						x = Integer.valueOf(keys[0]);
						y = Integer.valueOf(keys[1]);

						edgeList.insert(x, y, 0, 0);

					}
				}
              
				// looking for the U shape edges
				// how to find the ending points of the edge?
				e = edgeList.getKeys();

				first = false;
				startX = 0;
				startY = 0;
				endX = 0;
				endY = 0;

				endCount = 0;

				while (e.hasMoreElements()) {

					key = (String) e.nextElement();
					keys = key.split(":");

					x = Integer.valueOf(keys[0]);
					y = Integer.valueOf(keys[1]);

					degree = 0;
					currentX = 0;
					currentY = 0;

					if (edgeList.find(x - 1, y - 1) != null) {
						currentX = x - 1;
						currentY = y - 1;
						degree++;
					}

					if (edgeList.find(x, y - 1) != null) {
						currentX = x;
						currentY = y - 1;
						degree++;
					}

					if (edgeList.find(x + 1, y - 1) != null) {
						currentX = x + 1;
						currentY = y - 1;
						degree++;
					}

					if (edgeList.find(x - 1, y) != null) {
						currentX = x - 1;
						currentY = y;
						degree++;
					}

					if (edgeList.find(x + 1, y) != null) {
						currentX = x + 1;
						currentY = y;
						degree++;
					}

					if (edgeList.find(x - 1, y + 1) != null) {
						currentX = x - 1;
						currentY = y + 1;
						degree++;
					}

					if (edgeList.find(x, y + 1) != null) {
						currentX = x;
						currentY = y + 1;
						degree++;
					}

					if (edgeList.find(x + 1, y + 1) != null) {
						currentX = x + 1;
						currentY = y + 1;
						degree++;
					}

					if (degree == 1 && currentX != 0 && currentY != 0) {
						if (first == false) {
							startX = currentX;
							startY = currentY;
							first = true;
						} else {
							endX = currentX;
							endY = currentY;
						}
						endCount++;
					}
				} // end while

				// find the two ending points, then search the middle point
				if (endCount == 2) {

					// find the middle point
					e = edgeList.getKeys();

					endCount = 0;

					float minDist = 999;
					float leftLen, rightLen;
					float maxLen = 0;

					midX = 0;
					midY = 0;

					while (e.hasMoreElements()) {

						key = (String) e.nextElement();
						keys = key.split(":");

						x = Integer.valueOf(keys[0]);
						y = Integer.valueOf(keys[1]);

						leftLen = (float) Math.sqrt((x - startX) * (x - startX)
								+ (y - startY) * (y - startY));
						rightLen = (float) Math.sqrt((x - endX) * (x - endX)
								+ (y - endY) * (y - endY));

						minDist = Math.min(leftLen, rightLen);

						if (minDist > maxLen) {
							maxLen = minDist;
							midX = x;
							midY = y;
						}
					}

					// calculate the angle between the two lines.
					// angle theta < 120, U shape, delete the edge.
					if (midX != 0 && midY != 0) {
						float m1 = (startY - midY) / (startX - midX);
						float m2 = (endY - midY) / (endX - midX);

						// float tanTheta = (m1-m2)/(1+m1*m2);

						float angle = (float) Math.toDegrees(Math.atan2(
								(m1 - m2), (1 + m1 * m2)));
						if (angle < 0) {
							angle += 360;
						}
						angle = 360f - angle;

						if (angle < 110) {
							// edge.weight -= 3.0f;
							for (int k = 0; k < currentEdges.size(); k++) {
								edge = currentEdges.get(k);
								edge.weight -= 3.0f;
								edge.isUshape = true;
								// edgeTableCurrent.remove(edge);
							}

							// System.err.println("yes");
						}

					}

				}

			}

		}
	}

	public void enhanceOutmostBoundary() {

		float maxR, max2nd;
		Edge maxEdge = null;
		Edge max2ndEdge = null;
		Vector<Edge> edgeTableCurrent;

		for (int i = rotationAngleStart; i < rotationAngleEnd; i += scanDegree) {

			if (i >= 60 && i <= 120) {

				edgeTableCurrent = filteredEdgeRegionTable
						.get((i + ":" + (i + scanDegree)));

				Edge edge;
				maxR = 0;

				for (int ii = 0; ii < edgeTableCurrent.size(); ii++) {

					edge = (Edge) edgeTableCurrent.get(ii);

					if (edge.rAvg > maxR) {
						maxR = edge.rAvg;
						maxEdge = edge;
					}

				}

				max2nd = 0;
				for (int ii = 0; ii < edgeTableCurrent.size(); ii++) {

					edge = (Edge) edgeTableCurrent.get(ii);

					if (edge.rAvg > max2nd && edge.rAvg != maxR) {
						max2nd = edge.rAvg;
						max2ndEdge = edge;
					}

				}

				if (maxEdge != null && max2ndEdge != null) {
					// maxEdge.weight -= max2ndEdge.weight;
					max2ndEdge.weight -= 2.0f;
					maxEdge.weight -= 5.0f;
				}
			}
		}

	}

	public void sortEdges() {
		Vector<Edge> edgeTableCurrent;
		String key;
		String[] keys;
		String value;
		String[] values;
		int x, y;
		int count = 0;
		boolean first = false;
		float startX = 0, startY = 0;
		float endX = 0, endY = 0;
		float midX = 0, midY = 0;
		float currentX = 0, currentY = 0;
		int endCount = 0;

		for (int i = rotationAngleStart; i < rotationAngleEnd; i += scanDegree) {

			edgeTableCurrent = filteredEdgeRegionTable
					.get((i + ":" + (i + scanDegree)));

			Edge edge;

			for (int ii = 0; ii < edgeTableCurrent.size(); ii++) {

				first = false;
				startX = 0;
				startY = 0;
				endX = 0;
				endY = 0;
				edge = (Edge) edgeTableCurrent.get(ii);

				// how to find the ending points of the edge?
				Enumeration e = edge.getKeys();

				endCount = 0;

				while (e.hasMoreElements()) {

					key = (String) e.nextElement();
					keys = key.split(":");

					x = Integer.valueOf(keys[0]);
					y = Integer.valueOf(keys[1]);

					count = 0;
					currentX = 0;
					currentY = 0;

					if (edge.find(x - 1, y - 1) != null) {
						currentX = x - 1;
						currentY = y - 1;
						count++;
					}

					if (edge.find(x, y - 1) != null) {
						currentX = x;
						currentY = y - 1;
						count++;
					}

					if (edge.find(x + 1, y - 1) != null) {
						currentX = x + 1;
						currentY = y - 1;
						count++;
					}

					if (edge.find(x - 1, y) != null) {
						currentX = x - 1;
						currentY = y;
						count++;
					}

					if (edge.find(x + 1, y) != null) {
						currentX = x + 1;
						currentY = y;
						count++;
					}

					if (edge.find(x - 1, y + 1) != null) {
						currentX = x - 1;
						currentY = y + 1;
						count++;
					}

					if (edge.find(x, y + 1) != null) {
						currentX = x;
						currentY = y + 1;
						count++;
					}

					if (edge.find(x + 1, y + 1) != null) {
						currentX = x + 1;
						currentY = y + 1;
						count++;
					}

					if (count == 1 && currentX != 0 && currentY != 0) {
						if (first == false) {
							startX = currentX;
							startY = currentY;
							first = true;
						} else {
							endX = currentX;
							endY = currentY;
						}
						endCount++;
					}

				} // end while

				// find the two ending points, then search the middle point
				if (endCount == 2) {
					edge = (Edge) edgeTableCurrent.get(ii);

					// find the middle point
					e = edge.getKeys();

					endCount = 0;

					float minDist = 999;
					float leftLen, rightLen;
					float maxLen = 0;

					midX = 0;
					midY = 0;

					while (e.hasMoreElements()) {

						key = (String) e.nextElement();
						keys = key.split(":");

						x = Integer.valueOf(keys[0]);
						y = Integer.valueOf(keys[1]);

						leftLen = (float) Math.sqrt((x - startX) * (x - startX)
								+ (y - startY) * (y - startY));
						rightLen = (float) Math.sqrt((x - endX) * (x - endX)
								+ (y - endY) * (y - endY));

						minDist = Math.min(leftLen, rightLen);

						if (minDist > maxLen) {
							maxLen = minDist;
							midX = x;
							midY = y;
						}
					}

					// calculate the angle between the two lines.
					// angle theta < 120, U shape, delete the edge.
					if (midX != 0 && midY != 0) {
						float m1 = (startY - midY) / (startX - midX);
						float m2 = (endY - midY) / (endX - midX);

						// float tanTheta = (m1-m2)/(1+m1*m2);

						float angle = (float) Math.toDegrees(Math.atan2(
								(m1 - m2), (1 + m1 * m2)));

						if (angle < 0) {
							angle += 360;
						}

						if (angle < 110) {
							edge.weight -= 3.0f;
							// System.err.println("yes");
						}

					}

				}

			} // end for loop ii
		} // end for loop i

	}

	public void enhanceWeakLinkEdge(Hashtable<String, Vector<Edge>> edgeTable) {

		Vector<Edge> edgeTableCurrent;

		for (int i = rotationAngleStart; i < rotationAngleEnd; i += scanDegree) {

			edgeTableCurrent = edgeTable.get((i + ":" + (i + scanDegree)));
			
			Edge edgeCurrent;

			for (int ii = 0; ii < edgeTableCurrent.size(); ii++) {
				edgeCurrent = (Edge) edgeTableCurrent.get(ii);

				if (edgeCurrent.weight >= 0.1) {
					int degree = i;
					// search for the next edge connection
					while (edgeCurrent != null) {
						if (edgeCurrent.next != null) {
							degree += scanDegree;
							edgeCurrent = edgeCurrent.next;
						} else { // edgeCurrent.next == null;
							break;
						}
					}

					// looking for the current edge's weak link in the next contingent block
					findNearestRadiusEdge(edgeCurrent, (degree + scanDegree), edgeTable);
				}

			}
		}
	}

	public void findNearestRadiusEdge(Edge edgeCurrent, int startDegree, Hashtable<String, Vector<Edge>> edgeTable) {
		String key;
		String[] keys;
		String value;
		String[] values;
		int x, y;
		float r, theta;
		int count = 0;
		float radiusCurrent = 0f;
		float radiusNext = 0f;
		float radiusCurrentAvg = 0f;
		float radiusNextAvg = 0f;

		Edge edge;
		Edge bestEdge = null;

		float bestRNearest = 999;
		float bestRAvg = 999;

		Vector<Edge> edgeTableNext;

		float maxTheta = 0, minTheta = 999;
		float maxR = 0, minR = 0;
		// should not use the average. Average is wrong.
		// The radius should be the nearest point (r) between the two edges.
		// how to find the nearest point???

		Enumeration e = edgeCurrent.getKeys();
		while (e.hasMoreElements()) {

			key = (String) e.nextElement();
			keys = key.split(":");

			x = Integer.valueOf(keys[0]);
			y = Integer.valueOf(keys[1]);

			value = edgeCurrent.find(x, y);
			values = value.split(":");

			r = Float.valueOf(values[0]);
			theta = Float.valueOf(values[1]);

			if (theta > maxTheta) {
				maxTheta = theta;
				radiusCurrent = r;
			}

			radiusCurrentAvg += r;
			count++;
		}

		radiusCurrentAvg /= count;
		edgeCurrent.rNearest = radiusCurrent;
		edgeCurrent.rAvg = radiusCurrentAvg;

		// look ahead for two blocks.
		for (int ii = 0; ii < 4; ii++) {

			if (startDegree >= 360)
				startDegree -= 360;

			edgeTableNext = edgeTable
					.get((startDegree + ":" + (startDegree + scanDegree)));

			// findNearestEdge(edgeTableNext, edgeCurrent);

			if (edgeTableNext == null) {
				startDegree += scanDegree;
				continue;
			}

			// if ( startDegree > rotationAngleEnd ) break;

			for (int i = 0; i < edgeTableNext.size(); i++) {

				edge = (Edge) edgeTableNext.get(i);

				e = edge.getKeys();

				minTheta = 999;
				radiusNext = 0;
				radiusNextAvg = 0;
				count = 0;
				while (e.hasMoreElements()) {

					key = (String) e.nextElement();
					keys = key.split(":");

					x = Integer.valueOf(keys[0]);
					y = Integer.valueOf(keys[1]);

					value = edge.find(x, y);
					values = value.split(":");

					r = Float.valueOf(values[0]);
					theta = Float.valueOf(values[1]);

					if (theta < minTheta) {
						minTheta = theta;
						radiusNext = r;
					}

					radiusNextAvg += r;
					count++;
				}
				radiusNextAvg /= count;

				// System.err.println("radiusNext = " + radiusNext + "
				// radiusCurrent = " + radiusCurrent);

				edge.rNearest = radiusNext;
				edge.rAvg = radiusNextAvg;
				float weight = 0;
				if (Math.abs(edge.rNearest - edgeCurrent.rNearest) < bestRNearest) {
					bestRNearest = Math.abs(edge.rNearest - edgeCurrent.rNearest);
					bestEdge = edge;
					/*
					 * if ( Math.abs(edge.rAvg - edgeCurrent.rAvg) < 1.0f ) {
					 * weight = edgeCurrent.weight; } else { // weight =
					 * -edgeCurrent.weight; }
					 */

				}
			} // end for i

			startDegree += scanDegree;
		} // end for ii

		if (bestEdge != null) {

			float weight = edgeCurrent.weight;
			bestEdge.weight += weight;

			Edge current;
			current = bestEdge.next;
			while (current != null) {
				current.weight += weight;
				current = current.next;
				// System.err.println("add");
			}
			current = bestEdge.prev;
			while (current != null) {
				current.weight += weight;
				current = current.prev;
			}
		}
	}

	public void traceContinuity(Hashtable<String, Vector<Edge>> edgeTable) {

		int x, y;
		String value;
		String[] values;
		Vector<Edge> edgeTablePrevious;
		Vector<Edge> edgeTableCurrent;
		Vector<Edge> edgeTableNext;

		for (int i = rotationAngleStart; i < rotationAngleEnd; i += scanDegree) {

			edgeTableCurrent = edgeTable.get((i + ":" + (i + scanDegree)));

			if ((i - scanDegree) < 0) {
				//  System.err.println("Prev:   i = " + 351 + "  (i+scanDegree) = " + 360);
				edgeTablePrevious = edgeTable.get((351 + ":" + 360));
			} else {
				//  System.err.println("Prev:   i = " + (i-scanDegree) + "  i = " + (i));
				edgeTablePrevious = edgeTable.get(((i - scanDegree) + ":" + i));
			}

			if ((i + scanDegree) > 351) {
				// System.err.println("Next:   i = " + 0 + "  (i+scanDegree) = " + 9);
				edgeTableNext = edgeTable.get(0 + ":" + 9);
			} else {
				// System.err.println("Next:   i = " + (i+scanDegree) + "  (i+ 2*scanDegree) = " + (i + 2 * scanDegree));
				edgeTableNext = edgeTable.get((i + scanDegree) + ":" + (i + 2 * scanDegree));
			}
			
			Edge edgeCurrent;

		    
			if (edgeTableCurrent == null) {
				// System.err.println("test:   i = " + i + "  (i+scanDegree) = " + (i+scanDegree));
				continue;
			}
				

			for (int ii = 0; ii < edgeTableCurrent.size(); ii++) {
				edgeCurrent = (Edge) edgeTableCurrent.get(ii);

				Enumeration e = edgeCurrent.getKeys();

				while (e.hasMoreElements()) {

					value = (String) e.nextElement();
					values = value.split(":");

					x = Integer.valueOf(values[0]);
					y = Integer.valueOf(values[1]);

					// find neighbor on the previous scan region block

					findNeighbor(x - 1, y - 1, edgeTablePrevious, edgeCurrent,
							true);
					findNeighbor(x, y - 1, edgeTablePrevious, edgeCurrent, true);
					findNeighbor(x + 1, y - 1, edgeTablePrevious, edgeCurrent,
							true);
					findNeighbor(x - 1, y, edgeTablePrevious, edgeCurrent, true);
					findNeighbor(x + 1, y, edgeTablePrevious, edgeCurrent, true);
					findNeighbor(x - 1, y + 1, edgeTablePrevious, edgeCurrent,
							true);
					findNeighbor(x, y + 1, edgeTablePrevious, edgeCurrent, true);
					findNeighbor(x + 1, y + 1, edgeTablePrevious, edgeCurrent,
							true);

					/*
					 * findNeighbor(x - 2, y - 2, edgeTablePrevious,
					 * edgeCurrent,true); findNeighbor(x - 1, y - 2,
					 * edgeTablePrevious, edgeCurrent,true); findNeighbor(x, y -
					 * 2, edgeTablePrevious, edgeCurrent, true); findNeighbor(x +
					 * 1, y - 2, edgeTablePrevious, edgeCurrent, true);
					 * findNeighbor(x + 2, y - 2, edgeTablePrevious,
					 * edgeCurrent,true);
					 * 
					 * findNeighbor(x - 2, y, edgeTablePrevious, edgeCurrent,
					 * true); findNeighbor(x - 1, y, edgeTablePrevious,
					 * edgeCurrent, true); findNeighbor(x + 1, y,
					 * edgeTablePrevious, edgeCurrent, true); findNeighbor(x +
					 * 2, y, edgeTablePrevious, edgeCurrent, true);
					 * 
					 * findNeighbor(x - 2, y + 2, edgeTablePrevious,
					 * edgeCurrent,true); findNeighbor(x - 1, y + 2,
					 * edgeTablePrevious, edgeCurrent,true); findNeighbor(x, y +
					 * 2, edgeTablePrevious, edgeCurrent, true); findNeighbor(x +
					 * 1, y + 2, edgeTablePrevious, edgeCurrent,true);
					 * findNeighbor(x + 2, y + 2, edgeTablePrevious,
					 * edgeCurrent,true);
					 */

					// find the neighbor on the next scan region block
					findNeighbor(x - 1, y - 1, edgeTableNext, edgeCurrent,
							false);
					findNeighbor(x, y - 1, edgeTableNext, edgeCurrent, false);
					findNeighbor(x + 1, y - 1, edgeTableNext, edgeCurrent,
							false);
					findNeighbor(x - 1, y, edgeTableNext, edgeCurrent, false);
					findNeighbor(x + 1, y, edgeTableNext, edgeCurrent, false);
					findNeighbor(x - 1, y + 1, edgeTableNext, edgeCurrent,
							false);
					findNeighbor(x, y + 1, edgeTableNext, edgeCurrent, false);
					findNeighbor(x + 1, y + 1, edgeTableNext, edgeCurrent,
							false);

					/*
					 * 
					 * findNeighbor(x - 2, y - 2, edgeTableNext,
					 * edgeCurrent,false); findNeighbor(x - 1, y - 2,
					 * edgeTableNext, edgeCurrent,false); findNeighbor(x, y - 2,
					 * edgeTableNext, edgeCurrent, false); findNeighbor(x + 1, y -
					 * 2, edgeTableNext, edgeCurrent,false); findNeighbor(x + 2,
					 * y - 2, edgeTableNext, edgeCurrent, false);
					 * 
					 * findNeighbor(x - 2, y, edgeTableNext, edgeCurrent,
					 * false); findNeighbor(x - 1, y, edgeTableNext,
					 * edgeCurrent, false); findNeighbor(x + 1, y,
					 * edgeTableNext, edgeCurrent, false); findNeighbor(x + 2,
					 * y, edgeTableNext, edgeCurrent, false);
					 * 
					 * findNeighbor(x - 2, y + 2, edgeTableNext,
					 * edgeCurrent,false); findNeighbor(x - 1, y + 2,
					 * edgeTableNext, edgeCurrent,false); findNeighbor(x, y + 2,
					 * edgeTableNext, edgeCurrent, false); findNeighbor(x + 1, y +
					 * 2, edgeTableNext, edgeCurrent,false); findNeighbor(x + 2,
					 * y + 2, edgeTableNext, edgeCurrent,false);
					 */
				}

			}
		}

	}

	public boolean findNeighbor(int x, int y, Vector<Edge> _edgeTable,
			Edge edgeCurrent, boolean edgePrevFlag) {
		Edge edge;
		if (_edgeTable != null) {
			for (int ii = 0; ii < _edgeTable.size(); ii++) {
				edge = (Edge) _edgeTable.get(ii);

				if (edge.find(x, y) != null) {

					if (edgePrevFlag) {
						edgeCurrent.prev = edge;
					} else {
						edgeCurrent.next = edge;
					}

					return true;
				}
			}
		}
		return false;
	}

	// mark the end points
	public boolean trimPoints(Edge edge) {

		boolean found = false;
		// trace the edge to find the starting and ending points.
		// If the ending points for the edge is >= 3, ignore the edge by
		// reducing the weight.
		int x, y;

		Enumeration e = edge.getKeys();

		while (e.hasMoreElements()) {

			String key = (String) e.nextElement();
			String temp[] = key.split(":");

			int connectionCount = 0;

			x = Integer.valueOf(temp[0]);
			y = Integer.valueOf(temp[1]);

			int[] xCoord = new int[8];
			int[] yCoord = new int[8];
			int count = 0;

			if (edge.find(x - 1, y - 1) != null) {
				xCoord[count] = x - 1;
				yCoord[count] = y - 1;
				count++;
				connectionCount++;
			}

			if (edge.find(x, y - 1) != null) {
				xCoord[count] = x;
				yCoord[count] = y - 1;
				count++;
				connectionCount++;
			}

			if (edge.find(x + 1, y - 1) != null) {
				xCoord[count] = x + 1;
				yCoord[count] = y - 1;
				count++;
				connectionCount++;
			}

			if (edge.find(x - 1, y) != null) {
				xCoord[count] = x - 1;
				yCoord[count] = y;
				count++;
				connectionCount++;
			}

			if (edge.find(x + 1, y) != null) {
				xCoord[count] = x + 1;
				yCoord[count] = y;
				count++;
				connectionCount++;
			}

			if (edge.find(x - 1, y + 1) != null) {
				xCoord[count] = x - 1;
				yCoord[count] = y + 1;
				count++;
				connectionCount++;
			}

			if (edge.find(x, y + 1) != null) {
				xCoord[count] = x;
				yCoord[count] = y + 1;
				count++;
				connectionCount++;
			}

			if (edge.find(x + 1, y + 1) != null) {
				xCoord[count] = x + 1;
				yCoord[count] = y + 1;
				count++;
				connectionCount++;
			}

			if (connectionCount >= 5) {
				// make the all 9 pixels empty. and retrace the edge
				for (int k = 0; k < count; k++) {
					srcImage.set(xCoord[k], yCoord[k], 0);
				}
				srcImage.set(x, y, 0);
				found = true;
				break; // from the while loop.
			}
		} // end while
		return found;
	}

	public void traceOrientation(Vector<Edge> edgeTable, Vector<PolarPoint> voiPointsInRange) {
		int i;
		int count = 0;
		float voiSlopeAvg = 0;
		for (i = 0; i < voiPointsInRange.size() - 1; i++) {
			PolarPoint pt1 = voiPointsInRange.get(i);
			PolarPoint pt2 = voiPointsInRange.get(i + 1);
			if ((pt2.x - pt1.x) != 0) {
				voiSlopeAvg += (pt2.y - pt1.y) / (pt2.x - pt1.x);
				count++;
			} else {
				voiSlopeAvg += (pt2.y - pt1.y) / 0.1f;
				count++;
			}
		}
		voiSlopeAvg /= count;

		// System.err.println("voi slope = " + slopeAvg);

		float edgeSlopeAvg = 0;
		count = 0;
		for (i = 0; i < edgeTable.size(); i++) {

			Edge edge = (Edge) edgeTable.get(i);

			int x1, y1;
			int x2, y2;

			Enumeration e = edge.getKeys();

			while (e.hasMoreElements()) {

				String key = (String) e.nextElement();
				String temp[] = key.split(":");

				x1 = Integer.valueOf(temp[0]);
				y1 = Integer.valueOf(temp[1]);

				if (e.hasMoreElements()) {
					key = (String) e.nextElement();

					temp = key.split(":");

					x2 = Integer.valueOf(temp[0]);
					y2 = Integer.valueOf(temp[1]);

					if ((x2 - x1) != 0) {
						edgeSlopeAvg += (y2 - y1) / (x2 - x1);
						count++;
					} else {
						edgeSlopeAvg += (y2 - y1) / 0.1f;
						count++;
					}
				}

			}

			edgeSlopeAvg /= count;

			edge.orientation = edgeSlopeAvg;

			// System.err.println("edgeSlope = " + edgeSlope);

			if (edgeSlopeAvg >= (voiSlopeAvg - 0.1f)
					&& edgeSlopeAvg <= (voiSlopeAvg + 0.1f)) {
				// System.err.println("++");
				edge.weight += 0.1f;
			} else {
				// System.err.println("--");
				edge.weight -= 0.1f;
			}

			float receprocal = edgeSlopeAvg * voiSlopeAvg;

			if (receprocal >= (-1.0f - 0.3) && receprocal <= (-1.0f + 0.3f)) {
				edge.weight -= 1.0f;
			}

		}

		// rank each edge according to the nearest orientation.
		float nearest = 999;
		Edge maxEdge = null;
		for (i = 0; i < edgeTable.size(); i++) {
			Edge edge = (Edge) edgeTable.get(i);
			if ((edge.orientation - voiSlopeAvg) < nearest) {
				nearest = (edge.orientation - voiSlopeAvg);
				maxEdge = edge;
			}
		}

		if (maxEdge != null) {
			maxEdge.weight += 0.1;
		}

	}

	public void findLargestSpan() {
		float min = 999, max = -999;
		int x, y;
		float r, theta;
		float diff = 0;
		Edge edgeMaxSpan = null;
		for (int i = rotationAngleStart; i < rotationAngleEnd; i += scanDegree) {
			Vector<Edge> edgeTable = edgeRegionTable
					.get((i + ":" + (i + scanDegree)));
			Edge edge;
			for (int ii = 0; ii < edgeTable.size(); ii++) {
				edge = (Edge) edgeTable.get(ii);

				// for ( int jj = 0; jj < edge.pointsList.size(); jj++ ) {
				Enumeration e = edge.getKeys();

				while (e.hasMoreElements()) {

					String key = (String) e.nextElement();
					String temp[] = key.split(":");

					x = Integer.valueOf(temp[0]);
					y = Integer.valueOf(temp[1]);

					String value = edge.find(x, y);
					String[] values = value.split(":");

					r = Float.valueOf(values[0]);
					theta = Float.valueOf(values[1]);

					if (theta < min) {
						min = theta;
					}
					if (theta > max) {
						max = theta;
					}

				}

				if ((max - min) > diff) {
					diff = max - min;
					edgeMaxSpan = edge;
				}
			}
			edgeMaxSpan.weight += 0.1f;
		}
	}

	public void findVOIpointsInRange(Vector<PolarPoint> voiPointsInRange,
			int min, int max) {
		float r, theta;
		int x, y;

		Enumeration e = voiTable.getKeys();

		while (e.hasMoreElements()) {

			String key = (String) e.nextElement();

			String keys[] = key.split(":");

			x = Integer.valueOf(keys[0]);
			y = Integer.valueOf(keys[1]);

			String value = imageTable.find(x, y);

			String[] values = value.split(":");

			r = Float.valueOf(values[0]);
			theta = Float.valueOf(values[1]);

			if (theta >= min && theta <= max) {
				voiPointsInRange.add(new PolarPoint(x, y, r, theta));
			}

		}

	}

	public void findRegion(float theta1, float theta2,
			Vector<PolarPoint> voiPointsInRange) {
		int i;
		float r;
		float theta;
		float radius;
		int x, y;

		Enumeration e = imageTable.getKeys();

		while (e.hasMoreElements()) {

			String key = (String) e.nextElement();

			String keys[] = key.split(":");

			x = Integer.valueOf(keys[0]);
			y = Integer.valueOf(keys[1]);

			String value = imageTable.find(x, y);

			String[] values = value.split(":");

			radius = Float.valueOf(values[0]);
			theta = Float.valueOf(values[1]);

			if (theta >= theta1 && theta <= theta2) {
				for (int j = 0; j < voiPointsInRange.size(); j++) {
					PolarPoint voiPoint = (PolarPoint) voiPointsInRange.get(j);
					r = voiPoint.r;
					if (radius >= r - narrowBandWidthLower
							&& radius <= r + narrowBandWidthUpper) {
						narrowBandTable.insert(x, y, r, theta);
						visited.put((x + ":" + y), 0);
					}
				}
			}
		}
		// }
	}

	public void findRegion(float r1, float theta1, float r2, float theta2) {
		int i;
		float step;
		float theta;
		float radius;
		float r;
		int x, y;
		step = (r2 - r1) / 1f;

		// for (i = 0; i < imageTable.pointTable.size(); i++) {
		// PolarPoint point = (PolarPoint) (imageTable.pointTable.get(i));

		Enumeration e = imageTable.getKeys();

		while (e.hasMoreElements()) {

			String key = (String) e.nextElement();

			String keys[] = key.split(":");

			x = Integer.valueOf(keys[0]);
			y = Integer.valueOf(keys[1]);

			String value = imageTable.find(x, y);

			String[] values = value.split(":");

			radius = Float.valueOf(values[0]);
			theta = Float.valueOf(values[1]);

			if (theta >= theta1 && theta <= theta2) {
				for (r = r1; r != r2; r += step) {
					if (radius >= r - narrowBandWidthLower
							&& radius <= r + narrowBandWidthUpper) {
						narrowBandTable.insert(x, y, radius, theta);
						visited.put((x + ":" + y), 0);
					}
				}
			}
		}

	}

	public void createEdgeTable(Vector<Edge> edgeTable) {
		int i;
		Enumeration e = narrowBandTable.getKeys();
		int x, y;
		float theta, r;
		int visit = 0;
		PolarPoint seed;

		while (e.hasMoreElements()) {

			String key = (String) e.nextElement();
			String temp[] = key.split(":");

			x = Integer.valueOf(temp[0]);
			y = Integer.valueOf(temp[1]);

			if (srcImage.get(x, y).intValue() != 0) {
				Edge edge = new Edge();

				visit = (int) ((Integer) visited.get((x + ":" + y)));
				if (visit == 0) {
					// mark as visit
					Vector<String> visitMapCurrent = new Vector<String>();
					visited.put((x + ":" + y), 1);
					visitMapCurrent.add((x + ":" + y));

					String value = imageTable.find(x, y);

					String[] values = value.split(":");

					r = Float.valueOf(values[0]);
					theta = Float.valueOf(values[1]);

					seed = new PolarPoint(x, y, r, theta);

					// scan edge
					edgeScan(edge, seed, visitMapCurrent);

					if (trimPoints(edge)) {
						edge = new Edge();
						// roll back the current visit map
						for (int k = 0; k < visitMapCurrent.size(); k++) {
							String keyValue = (String) visitMapCurrent.get(k);
							visited.put(keyValue, 0);
						}
						visitMapCurrent.clear();
						// re-scan
						edgeScan(edge, seed, visitMapCurrent);
					}

					
					trimEdge(edge);
					
					if (edge.size() != 0) {
						edgeTable.add(edge);
					}
				}
			}

		}

	}
	
	public void trimEdge(Edge edge) {
		int x,y;
		float r, theta;
		int degree;
		
		Enumeration e = edge.getKeys();
		
		int upperLeft, upper, upperRight;
		int left, center, right;
		int lowerLeft, lower, lowerRight;

		Vector<String> result = new Vector<String>();
		
		while ( e.hasMoreElements() ) {
			String key = (String) e.nextElement();
			String temp[] = key.split(":");

			x = Integer.valueOf(temp[0]);
			y = Integer.valueOf(temp[1]);

			degree = 0;

			upperLeft = srcImage.get(x - 1, y - 1).intValue();
			if (upperLeft != 0) {
				degree++;
			}

			upper = srcImage.get(x, y - 1).intValue();
			if (upper != 0) {
				degree++;
			}

			upperRight = srcImage.get(x + 1, y - 1).intValue();
			if (upperRight != 0) {
				degree++;
			}

			left = srcImage.get(x - 1, y).intValue();
			if (left != 0) {
				degree++;
			}

			right = srcImage.get(x + 1, y).intValue();
			if (right != 0) {
				degree++;
			}

			lowerLeft = srcImage.get(x - 1, y + 1).intValue();
			if (lowerLeft != 0) {
				degree++;
			}

			lower = srcImage.get(x, y + 1).intValue();
			if (lower != 0) {
				degree++;
			}

			lowerRight = srcImage.get(x + 1, y + 1).intValue();
			if (lowerRight != 0) {
				degree++;
			}
			if (degree == 0) {
				srcImage.set(x, y, 0);
				result.add(x + ":" + y);
			} else if (degree == 1) {
				srcImage.set(x, y, 0);
				result.add(x + ":" + y);
			} else if (degree == 2) {

				if (upperLeft != 0 && upper != 0) {
					srcImage.set(x, y, 0);
					result.add(x + ":" + y);
				} else if (upper != 0 && upperRight != 0) {
					srcImage.set(x, y, 0);
					result.add(x + ":" + y);
				} else if (upperRight != 0 && right != 0) {
					srcImage.set(x, y, 0);
					result.add(x + ":" + y);
				} else if (right != 0 && lowerRight != 0) {
					srcImage.set(x, y, 0);
					result.add(x + ":" + y);
				} else if (lowerRight != 0 && lower != 0) {
					srcImage.set(x, y, 0);
					result.add(x + ":" + y);
				} else if (lower != 0 && lowerLeft != 0) {
					srcImage.set(x, y, 0);
					result.add(x + ":" + y);
				} else if (lowerLeft != 0 && left != 0) {
					srcImage.set(x, y, 0);
					result.add(x + ":" + y);
				} else if (left != 0 && upperLeft != 0) {
					srcImage.set(x, y, 0);
					result.add(x + ":" + y);
				}
			} else {
				if (left != 0 && upperLeft != 0 && upper != 0) {
					srcImage.set(x - 1, y - 1, 0);
					result.add((x-1) + ":" + (y-1));
				}
				if (upper != 0 && upperRight != 0 && right != 0) {
					srcImage.set(x + 1, y - 1, 0);
					result.add((x+1) + ":" + (y-1));
				}

				if (right != 0 && lowerRight != 0 && lower != 0) {
					srcImage.set(x + 1, y + 1, 0);
					result.add((x+1) + ":" + (y+1));
				}

				if (lower != 0 && lowerLeft != 0 && left != 0) {
					srcImage.set(x - 1, y + 1, 0);
					result.add((x-1) + ":" + (y+1));
				}
			}
				
		} // end while
		
		for ( int i = 0; i < result.size(); i++ ) {
			edge.remove(result.get(i));
		}
		
	}

	public void edgeScan(Edge edge, PolarPoint seed, Vector<String> visitMapCurrent) {
		int x, y;
		int value;
		String hashValue;
		String[] hashValues;
		float r, theta;
		ArrayDeque<PolarPoint> q = new ArrayDeque<PolarPoint>();

		q.offerLast(seed);

		while (!q.isEmpty()) {
			seed = q.getFirst();
			edge.insert(seed.x, seed.y, seed.r, seed.theta);
			q.removeFirst();
			// test the 8 neighbors
			x = seed.x;
			y = seed.y;

			if (inRange(x - 1, y - 1)) { // top left pixel
				value = (Integer) visited.get((x - 1) + ":" + (y - 1));
				if (value == 0) {
					visited.put(((x - 1) + ":" + (y - 1)), 1);
					visitMapCurrent.add(((x - 1) + ":" + (y - 1)));
					if (srcImage.get(x - 1, y - 1).intValue() != 0) {
						hashValue = imageTable.find(x - 1, y - 1);
						hashValues = hashValue.split(":");
						r = Float.valueOf(hashValues[0]);
						theta = Float.valueOf(hashValues[1]);
						q.offerLast(new PolarPoint(x - 1, y - 1, r, theta));
					}
				}
			}

			if (inRange(x, y - 1)) { // top middle pixel
				value = (Integer) visited.get(x + ":" + (y - 1));
				if (value == 0) {
					visited.put((x + ":" + (y - 1)), 1);
					visitMapCurrent.add((x + ":" + (y - 1)));
					if (srcImage.get(x, y - 1).intValue() != 0) {
						hashValue = imageTable.find(x, y - 1);
						hashValues = hashValue.split(":");
						r = Float.valueOf(hashValues[0]);
						theta = Float.valueOf(hashValues[1]);
						q.offerLast(new PolarPoint(x, y - 1, r, theta));
					}
				}
			}

			if (inRange(x + 1, y - 1)) { // top right pixel
				value = (Integer) visited.get((x + 1) + ":" + (y - 1));
				if (value == 0) {
					visited.put(((x + 1) + ":" + (y - 1)), 1);
					visitMapCurrent.add(((x + 1) + ":" + (y - 1)));
					if (srcImage.get(x + 1, y - 1).intValue() != 0) {
						hashValue = imageTable.find(x + 1, y - 1);
						hashValues = hashValue.split(":");
						r = Float.valueOf(hashValues[0]);
						theta = Float.valueOf(hashValues[1]);
						q.offerLast(new PolarPoint(x + 1, y - 1, r, theta));
					}
				}
			}

			if (inRange(x - 1, y)) { // left pixel
				value = (Integer) visited.get((x - 1) + ":" + y);
				if (value == 0) {
					visited.put(((x - 1) + ":" + y), 1);
					visitMapCurrent.add(((x - 1) + ":" + y));
					if (srcImage.get(x - 1, y).intValue() != 0) {
						hashValue = imageTable.find(x - 1, y);
						hashValues = hashValue.split(":");
						r = Float.valueOf(hashValues[0]);
						theta = Float.valueOf(hashValues[1]);
						q.offerLast(new PolarPoint(x - 1, y, r, theta));
					}
				}
			}

			if (inRange(x + 1, y)) { // right pixel
				value = (Integer) visited.get((x + 1) + ":" + y);
				if (value == 0) {
					visited.put(((x + 1) + ":" + y), 1);
					visitMapCurrent.add(((x + 1) + ":" + y));
					if (srcImage.get(x + 1, y).intValue() != 0) {
						hashValue = imageTable.find(x + 1, y);
						hashValues = hashValue.split(":");
						r = Float.valueOf(hashValues[0]);
						theta = Float.valueOf(hashValues[1]);
						q.offerLast(new PolarPoint(x + 1, y, r, theta));
					}
				}
			}

			if (inRange(x - 1, y + 1)) { // lower left pixel
				value = (Integer) visited.get((x - 1) + ":" + (y + 1));
				if (value == 0) {
					visited.put(((x - 1) + ":" + (y + 1)), 1);
					visitMapCurrent.add(((x - 1) + ":" + (y + 1)));
					if (srcImage.get(x - 1, y + 1).intValue() != 0) {
						hashValue = imageTable.find(x - 1, y + 1);
						hashValues = hashValue.split(":");
						r = Float.valueOf(hashValues[0]);
						theta = Float.valueOf(hashValues[1]);
						q.offerLast(new PolarPoint(x - 1, y + 1, r, theta));
					}
				}

			}

			if (inRange(x, y + 1)) { // lower middle pixel
				value = (Integer) visited.get(x + ":" + (y + 1));
				if (value == 0) {
					visited.put((x + ":" + (y + 1)), 1);
					visitMapCurrent.add((x + ":" + (y + 1)));
					if (srcImage.get(x, y + 1).intValue() != 0) {
						hashValue = imageTable.find(x, y + 1);
						hashValues = hashValue.split(":");
						r = Float.valueOf(hashValues[0]);
						theta = Float.valueOf(hashValues[1]);
						q.offerLast(new PolarPoint(x, y + 1, r, theta));
					}
				}

			}

			if (inRange(x + 1, y + 1)) { // lower right pixel
				value = (Integer) visited.get((x + 1) + ":" + (y + 1));
				if (value == 0) {
					visited.put(((x + 1) + ":" + (y + 1)), 1);
					visitMapCurrent.add(((x + 1) + ":" + (y + 1)));
					if (srcImage.get(x + 1, y + 1).intValue() != 0) {
						hashValue = imageTable.find(x + 1, y + 1);
						hashValues = hashValue.split(":");
						r = Float.valueOf(hashValues[0]);
						theta = Float.valueOf(hashValues[1]);
						q.offerLast(new PolarPoint(x + 1, y + 1, r, theta));
					}
				}

			}
		}

	}

	public boolean inRange(int x, int y) {
		String value = (String) narrowBandTable.find(x, y);
		if (value == null)
			return false;
		else
			return true;
	}

	public void mergeBoundary() {

		int x, y;
		float r, theta;

		for (int i = rotationAngleStart; i < rotationAngleEnd; i += scanDegree) {

			Vector<Edge> edgeTable = finalEdgeRegionTable
					.get((i + ":" + (i + scanDegree)));

			Vector<Edge> addTable = addEdgeTable
					.get((i + ":" + (i + scanDegree)));

			Edge edge;
			Edge add;

			if (addTable != null) {

				if (edgeTable == null) {
					edgeTable = new Vector<Edge>();
					edge = new Edge();
					edgeTable.add(edge);
				} else {
					edge = (Edge) edgeTable.get(0);
				}
				add = (Edge) addTable.get(0);

				if (edge != null) {

					Enumeration e = add.getKeys();

					while (e.hasMoreElements()) {

						String key = (String) e.nextElement();
						String temp[] = key.split(":");

						x = Integer.valueOf(temp[0]);
						y = Integer.valueOf(temp[1]);

						String value = add.find(x, y);
						String[] values = value.split(":");

						r = Float.valueOf(values[0]);
						theta = Float.valueOf(values[1]);

						edge.insert(x, y, r, theta);
					}
				}

			}

		}

	}

	public void viewEdgeMap() {
		int position;

		int xDim = srcImage.getExtents()[0];
		int yDim = srcImage.getExtents()[1];
		int sliceSize = xDim * yDim;
		double[] sourceBuffer = new double[sliceSize];
		double[] originalBuffer = new double[sliceSize];

		try {

			srcImage.exportData(0, sliceSize, sourceBuffer);
			srcImage.exportData(0, sliceSize, originalBuffer);

			for (int y = 0; y < yDim; y++) {
				for (int x = 0; x < xDim; x++) {
					position = x + y * xDim;
					sourceBuffer[position] = 0;
				}
			}

			int x, y;
			float r1, r2, theta1, theta2;

			float min = 999, max = -999;
			float diff = 0;

			for (int i = rotationAngleStart; i < rotationAngleEnd; i += scanDegree) {

				/*
				 * for (y = 0; y < yDim; y++) { for (x = 0; x < xDim; x++) {
				 * position = x + y * xDim; sourceBuffer[position] = 0; } }
				 */

				Vector<Edge> edgeTable = finalEdgeRegionTable.get((i + ":" + (i + scanDegree)));

				Edge edge;

				if (edgeTable != null) {

			      for (int k = 0; k < edgeTable.size(); k++) {
						edge = (Edge) edgeTable.get(k);

						if (edge != null ) {

							Enumeration e = edge.getKeys();

							while (e.hasMoreElements()) {

								String key = (String) e.nextElement();
								String temp[] = key.split(":");

								x = Integer.valueOf(temp[0]);
								y = Integer.valueOf(temp[1]);

								position = x + y * xDim;
								sourceBuffer[position] = 250;
							}
						}
				  }

				}

				// pause();
				// System.err.println((i + ":" + (i+scanDegree)));

				srcImage.importData(0, sourceBuffer, true);
				srcImage.calcMinMax();
				srcImage.notifyImageDisplayListeners(null, true);

			}

		} catch (FileNotFoundException err) {
			err.printStackTrace();
		} catch (IOException err) {
			err.printStackTrace();
		}

	}

	
	public void viewImage() {
		int position;

		int xDim = srcImage.getExtents()[0];
		int yDim = srcImage.getExtents()[1];
		int sliceSize = xDim * yDim;
		double[] sourceBuffer = new double[sliceSize];
		double[] originalBuffer = new double[sliceSize];

		try {

			srcImage.exportData(0, sliceSize, sourceBuffer);
			srcImage.exportData(0, sliceSize, originalBuffer);

			for (int y = 0; y < yDim; y++) {
				for (int x = 0; x < xDim; x++) {
					position = x + y * xDim;
					sourceBuffer[position] = 0;
				}
			}

			int x, y;
			float r1, r2, theta1, theta2;

			float min = 999, max = -999;
			float diff = 0;

			for (int i = rotationAngleStart; i < rotationAngleEnd; i += scanDegree) {

				 /*
				 for (y = 0; y < yDim; y++) { 
					 for (x = 0; x < xDim; x++) {
						 position = x + y * xDim; 
						 sourceBuffer[position] = 0; 
					 } 
				 }
				*/

				Vector<Edge> edgeTable = finalEdgeRegionTable.get((i + ":" + (i + scanDegree)));

				
				Edge edge;

				if (edgeTable != null) {

			    //   for (int k = 0; k < edgeTable.size(); k++) {
						edge = (Edge) edgeTable.get(0);

						if (edge != null) {

							Enumeration e = edge.getKeys();

							while (e.hasMoreElements()) {

								String key = (String) e.nextElement();
								String temp[] = key.split(":");

								x = Integer.valueOf(temp[0]);
								y = Integer.valueOf(temp[1]);

								position = x + y * xDim;
								sourceBuffer[position] = 250;
							}
						}
				// 	}

				}

				// pause();
				// System.err.println((i + ":" + (i+scanDegree)));

				srcImage.importData(0, sourceBuffer, true);
				srcImage.calcMinMax();
				srcImage.notifyImageDisplayListeners(null, true);

			}

		} catch (FileNotFoundException err) {
			err.printStackTrace();
		} catch (IOException err) {
			err.printStackTrace();
		}

	}

	public void viewAddEdge() {
		int position;

		int xDim = srcImage.getExtents()[0];
		int yDim = srcImage.getExtents()[1];
		int sliceSize = xDim * yDim;
		double[] sourceBuffer = new double[sliceSize];
		double[] originalBuffer = new double[sliceSize];

		try {

			srcImage.exportData(0, sliceSize, sourceBuffer);
			srcImage.exportData(0, sliceSize, originalBuffer);

			/*
			 * for (int y = 0; y < yDim; y++) { for (int x = 0; x < xDim; x++) {
			 * position = x + y * xDim; sourceBuffer[position] = 0; } }
			 */

			int x, y;
			float r1, r2, theta1, theta2;

			float min = 999, max = -999;
			float diff = 0;

			for (int i = rotationAngleStart; i < rotationAngleEnd; i += scanDegree) {

				/*
				 * for (y = 0; y < yDim; y++) { for (x = 0; x < xDim; x++) {
				 * position = x + y * xDim; sourceBuffer[position] = 0; } }
				 */

				Vector<Edge> edgeTable = addEdgeTable.get((i + ":" + (i + scanDegree)));

				Edge edge;

				if (edgeTable != null) {

					edge = (Edge) edgeTable.get(0);

					if (edge != null) {
						Enumeration e = edge.getKeys();

						while (e.hasMoreElements()) {

							String key = (String) e.nextElement();
							String temp[] = key.split(":");

							x = Integer.valueOf(temp[0]);
							y = Integer.valueOf(temp[1]);

							position = x + y * xDim;
							sourceBuffer[position] = 250;
						}
					}

				}

				// pause();
				// System.err.println((i + ":" + (i+scanDegree)));

				srcImage.importData(0, sourceBuffer, true);
				srcImage.calcMinMax();
				srcImage.notifyImageDisplayListeners(null, true);

			}

		} catch (FileNotFoundException err) {
			err.printStackTrace();
		} catch (IOException err) {
			err.printStackTrace();
		}

	}

	public void updateFinalEdge() {
		int position;

		int xDim = srcImage.getExtents()[0];
		int yDim = srcImage.getExtents()[1];
		int sliceSize = xDim * yDim;
		double[] sourceBuffer = new double[sliceSize];
		double[] originalBuffer = new double[sliceSize];

		try {

			srcImage.exportData(0, sliceSize, sourceBuffer);
			srcImage.exportData(0, sliceSize, originalBuffer);

			for (int y = 0; y < yDim; y++) {
				for (int x = 0; x < xDim; x++) {
					position = x + y * xDim;
					sourceBuffer[position] = 0;
				}
			}

			int x, y;
			float r1, r2, theta1, theta2;

			float min = 999, max = -999;
			float diff = 0;

			for (int i = rotationAngleStart; i < rotationAngleEnd; i += scanDegree) {

				/*
				 * for (y = 0; y < yDim; y++) { for (x = 0; x < xDim; x++) {
				 * position = x + y * xDim; sourceBuffer[position] = 0; } }
				 */

				Vector<Edge> edgeTable = finalTable
						.get((i + ":" + (i + scanDegree)));

				Edge edge;

				if (edgeTable != null) {

					edge = (Edge) edgeTable.get(0);

					if (edge != null) {
						Enumeration e = edge.getKeys();

						while (e.hasMoreElements()) {

							String key = (String) e.nextElement();
							String temp[] = key.split(":");

							x = Integer.valueOf(temp[0]);
							y = Integer.valueOf(temp[1]);

							position = x + y * xDim;
							sourceBuffer[position] = 250;
						}
					}

				}

				// pause();
				// System.err.println((i + ":" + (i+scanDegree)));

				srcImage.importData(0, sourceBuffer, true);
				srcImage.calcMinMax();
				srcImage.notifyImageDisplayListeners(null, true);

			}

		} catch (FileNotFoundException err) {
			err.printStackTrace();
		} catch (IOException err) {
			err.printStackTrace();
		}

	}

	/**
	 * Pauses the display until the user hits enter.
	 */
	public static void pause() {
		int count = 0;

		try {
			// eat any pending characters
			for (int av = System.in.available(); av > 0; av--) {
				System.in.read();
			}
			System.in.read();// wait for user to hit Enter, discard result
		} catch (IOException e) {
			System.err.println("keyboard failed: " + e);
		}

	}

	public void createImagePointTable() {

		int xDim = srcImage.getExtents()[0];
		int yDim = srcImage.getExtents()[1];

		float r, theta;

		for (int x = 0; x < xDim; x++) {
			for (int y = 0; y < yDim; y++) {
				Vector2f in = new Vector2f(x, y);
				Vector2f out = new Vector2f(0, 0);

				MipavCoordinateSystems.CartesianToPolar2D(in, out, srcImage);

				r = out.X;
				theta = out.Y;

				imageTable.insert(x, y, r, theta);

				// in = null;
				// out = null;

			}
		}

		/*
		 * Vector2f in = new Vector2f(262, 90); Vector2f out = new Vector2f(0,
		 * 0);
		 *  // check cartesian to polar;
		 * MipavCoordinateSystems.CartesianToPolar2D(in, out, srcImage); r =
		 * out.X; theta = out.Y; System.err.println("Point1: x = 261, y = 90" + "
		 * r = " + r + " theta = " + theta);
		 * 
		 * in = new Vector2f(261, 182);
		 * MipavCoordinateSystems.CartesianToPolar2D(in, out, srcImage); r =
		 * out.X; theta = out.Y; System.err.println("Point2: x = 261, y = 181" + "
		 * r = " + r + " theta = " + theta);
		 * 
		 * 
		 * in = new Vector2f(131, 181);
		 * MipavCoordinateSystems.CartesianToPolar2D(in, out, srcImage); r =
		 * out.X; theta = out.Y; System.err.println("Point3: x = 130, y = 181" + "
		 * r = " + r + " theta = " + theta);
		 * 
		 * in = new Vector2f(0, 181);
		 * MipavCoordinateSystems.CartesianToPolar2D(in, out, srcImage); r =
		 * out.X; theta = out.Y; System.err.println("Point4: x = 0, y = 181" + "
		 * r = " + r + " theta = " + theta);
		 * 
		 * in = new Vector2f(0, 90);
		 * MipavCoordinateSystems.CartesianToPolar2D(in, out, srcImage); r =
		 * out.X; theta = out.Y; System.err.println("Point5: x = 0, y = 90" + "
		 * r = " + r + " theta = " + theta);
		 * 
		 * in = new Vector2f(0, 0);
		 * MipavCoordinateSystems.CartesianToPolar2D(in, out, srcImage); r =
		 * out.X; theta = out.Y; System.err.println("Point6: x = 0, y = 0" + " r = " +
		 * r + " theta = " + theta);
		 * 
		 * in = new Vector2f(130, 0);
		 * MipavCoordinateSystems.CartesianToPolar2D(in, out, srcImage); r =
		 * out.X; theta = out.Y; System.err.println("Point7: x = 130, y = 0" + "
		 * r = " + r + " theta = " + theta);
		 * 
		 * in = new Vector2f(261, 0);
		 * MipavCoordinateSystems.CartesianToPolar2D(in, out, srcImage); r =
		 * out.X; theta = out.Y; System.err.println("Point8: x = 261, y = 0" + "
		 * r = " + r + " theta = " + theta);
		 * 
		 * 
		 * Vector3f center = srcImage.getImageCenter();
		 * 
		 * float centerX = center.X; float centerY = center.Y;
		 * 
		 * in = new Vector2f(centerX, centerY);
		 * MipavCoordinateSystems.CartesianToPolar2D(in, out, srcImage); r =
		 * out.X; theta = out.Y; System.err.println("Point9: x = " + centerX + ",
		 * y = " + centerY + " r = " + r + " theta = " + theta);
		 */

		/*
		 * // check polar to cartisian; int x, y; in = new Vector2f(131, 0);
		 * MipavCoordinateSystems.PolarToCartesian2D(in, out, srcImage); x =
		 * (int)out.X; y = (int)out.Y; System.err.println("Point0: r = 131,
		 * theta = 0" + " x = " + x + " y = " + y);
		 * 
		 * in = new Vector2f(160, 45);
		 * MipavCoordinateSystems.PolarToCartesian2D(in, out, srcImage); x =
		 * (int)out.X; y = (int)out.Y; System.err.println("Point1: r = 160,
		 * theta = 45" + " x = " + x + " y = " + y);
		 * 
		 * in = new Vector2f(90, 90);
		 * MipavCoordinateSystems.PolarToCartesian2D(in, out, srcImage); x =
		 * (int)out.X; y = (int)out.Y; System.err.println("Point2: r = 90, theta =
		 * 90" + " x = " + x + " y = " + y);
		 * 
		 * in = new Vector2f(160, 135);
		 * MipavCoordinateSystems.PolarToCartesian2D(in, out, srcImage); x =
		 * (int)out.X; y = (int)out.Y; System.err.println("Point3: r = 160,
		 * theta = 135" + " x = " + x + " y = " + y);
		 * 
		 * in = new Vector2f(131, 180);
		 * MipavCoordinateSystems.PolarToCartesian2D(in, out, srcImage); x =
		 * (int)out.X; y = (int)out.Y; System.err.println("Point4: r = 131,
		 * theta = 180" + " x = " + x + " y = " + y);
		 * 
		 * in = new Vector2f(160, 225);
		 * MipavCoordinateSystems.PolarToCartesian2D(in, out, srcImage); x =
		 * (int)out.X; y = (int)out.Y; System.err.println("Point5: r = 160,
		 * theta = 225" + " x = " + x + " y = " + y);
		 * 
		 * in = new Vector2f(91, 270);
		 * MipavCoordinateSystems.PolarToCartesian2D(in, out, srcImage); x =
		 * (int)out.X; y = (int)out.Y; System.err.println("Point6: r = 91, theta =
		 * 270" + " x = " + x + " y = " + y);
		 * 
		 * in = new Vector2f(160, 315);
		 * MipavCoordinateSystems.PolarToCartesian2D(in, out, srcImage); x =
		 * (int)out.X; y = (int)out.Y; System.err.println("Point7: r = 160,
		 * theta = 315" + " x = " + x + " y = " + y);
		 * 
		 * in = new Vector2f(0, 0);
		 * MipavCoordinateSystems.PolarToCartesian2D(in, out, srcImage); x =
		 * (int)out.X; y = (int)out.Y; System.err.println("Point8: r = 0, theta =
		 * 0" + " x = " + x + " y = " + y);
		 * 
		 * 
		 * if ( true ) { System.exit(0); }
		 */
	}

	private void createVOIPointTable() {

		int zDim = 1;
		int x, y;
		float r, theta;
		int i;
		if (srcImage.getVOIs() != null) {
			this.VOIs = srcImage.getVOIs();
		}

		// find the intersection of the lower bound with the VOI.
		Vector<VOIBase>[] vArray = VOIs.VOIAt(0).getSortedCurves(
				VOIBase.ZPLANE, zDim);
		VOIBase v = vArray[0].get(0);

		int nPts = v.size();

		for (i = 0; i < nPts; i++) {
			x = (int) ((Vector3f) (v.elementAt(i))).X;
			y = (int) ((Vector3f) (v.elementAt(i))).Y;

			Vector2f in = new Vector2f(x, y);
			Vector2f out = new Vector2f(0, 0);

			MipavCoordinateSystems.CartesianToPolar2D(in, out, srcImage);

			r = out.X;
			theta = out.Y;

			voiTable.insert(x, y, r, theta);

			// in = null;
			// out = null;

		}

		setCompleted(true);
	}

}

class PolarPoint implements Comparator {
	float r; // Polar coordinate R
	float theta; // Polar coordinate theta
	int x; // Relative pixel coordinate x
	int y; // Relative pixel coordinate y

	public PolarPoint() {

	}

	public PolarPoint(int _x, int _y, float _r, float _theta) {
		x = _x;
		y = _y;
		r = _r;
		theta = _theta;
	}

	public void assign(PolarPoint p) {
		this.x = p.x;
		this.y = p.y;
		this.r = p.r;
		this.theta = p.theta;
	}

	public int compare(final Object v1, final Object v2) {
		PolarPoint a, b;

		a = (PolarPoint) v1;
		b = (PolarPoint) v2;

		float va, vb;

		va = a.theta;
		vb = b.theta;

		int first = va > vb ? 1 : 0;
		int second = va < vb ? 1 : 0;

		return first - second;

	}

}

class Edge {

	float weight;
	float rNearest;
	float rAvg;
	float orientation;
	int orientationRank;
	boolean visited = false;
	boolean isUshape = false;

	Edge prev;
	Edge next;

	Hashtable table = new Hashtable();

	public int size() {
		return table.size();
	}

	public void insert(int x, int y, float r, float theta) {
		String key = x + ":" + y;
		String value = r + ":" + theta;
		table.put(key, value);
	}

	public String find(int x, int y) {
		String value;
		String key = x + ":" + y;
		value = (String) table.get((String) key);
		return value;
	}

	public Enumeration getKeys() {
		return table.keys();
	}

	public void clear() {
		table.clear();
	}

	public void remove(String key) {
		table.remove(key);
	}
}
