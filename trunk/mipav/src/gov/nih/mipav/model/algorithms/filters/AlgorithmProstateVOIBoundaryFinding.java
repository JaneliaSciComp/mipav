package gov.nih.mipav.model.algorithms.filters;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewVOIVector;
import gov.nih.mipav.*;
import gov.nih.mipav.util.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree.*;
import gov.nih.mipav.model.structures.*;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.*;
import java.awt.geom.*;

import gov.nih.mipav.model.algorithms.filters.AlgorithmProstateBoundaryFinding.*;

import WildMagic.LibFoundation.Mathematics.*;

public class AlgorithmProstateVOIBoundaryFinding extends AlgorithmBase implements
		AlgorithmInterface {

	private VOI resultVOI;

	private VOIVector VOIs;

	private Edge voiTable = new Edge();

	private Edge imageTable = new Edge();
	
	private Hashtable<String, Vector<Edge>> finalTable = new Hashtable<String, Vector<Edge>>();

	public AlgorithmProstateVOIBoundaryFinding(ModelImage _srcImage) {
		srcImage = (ModelImage)_srcImage.clone();
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

    	finalTable.clear();
    	finalTable = null;
    
    
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
	    // createImagePointTable();
		createVOIPointTable();
		sortPoints();
		setCompleted(true);
		System.gc();
		// viewEdgeMap();
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


	
	

	public void sortPoints() {
		int x, y;
		float r, theta;
		 Vector contours;
	        int nVOI, nContours;
	        
		Vector<Vector3f> result = new Vector<Vector3f>();

		
			Vector<PolarPoint> pointVector = new Vector<PolarPoint>();

			
            Enumeration e = voiTable.table.keys();
	        
            while ( e.hasMoreElements() ) {
    			String key = (String) e.nextElement();
    			String temp[] = key.split(":");

    			x = Integer.valueOf(temp[0]);
    			y = Integer.valueOf(temp[1]);
    			
	            String value = voiTable.find(x, y);
				String[] values = value.split(":");

				r = Float.valueOf(values[0]);
				theta = Float.valueOf(values[1]);
	            
	            pointVector.add(new PolarPoint(x, y, r, theta));
	            
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
				// if (j % 2 == 0) {
					
					p = points[j];
					
					if ( !isInteriorPoint(p, points, j)) {
						result.add(new Vector3f(p.x, p.y, 0f));
				    }
				// }
			}

		int len = result.size();

		Vector3f[] voiList = new Vector3f[len];

		for (int i = 0; i < len; i++) {
			voiList[i] = result.get(i);
		}

		resultVOI = new VOI((short) srcImage.getVOIs().size(), "prostate", VOI.CONTOUR, -1.0f);
		resultVOI.importCurve(voiList);
		

	}

	public boolean isExteriorPoint(PolarPoint p, PolarPoint[] points, int index) {
		boolean isExteriorPoint = false;
		
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
		if ( p.r >= (avg_r - 0.5) ) isExteriorPoint = true;
		return isExteriorPoint;
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
				
			    if( theta >= (theta_p - 7) && theta <= (theta_p+7) ) { 
			    	
			    		avg_r += currentPoint.r;
			    		count++;
			    }
				
			}
		}
		
		avg_r /= count;
		if ( p.r <= (avg_r - 0.5) ) isInteriorPoint = true;
		return isInteriorPoint;
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
        int intensity;
		float r, theta;

		for (int x = 0; x < xDim; x++) {
			for (int y = 0; y < yDim; y++) {
				Vector2f in = new Vector2f(x, y);
				Vector2f out = new Vector2f(0, 0);

				MipavCoordinateSystems.CartesianToPolar2D(in, out, srcImage);

				r = out.X;
				theta = out.Y;
				intensity = srcImage.get(x, y).intValue();

				if (intensity != 0) {
                   imageTable.insert(x, y, r, theta);
				}

			}
		}

		
	}
	
	public void createVOIPointTable() {
		float r, theta;

		Vector<VOIBase>[] vArray = srcImage.getVOIs().VOIAt(0).getSortedCurves(VOIBase.ZPLANE, 1);
		VOIBase v= vArray[0].get(0);
		
		int nPts = v.size();
		
		System.err.println("nPts = " + nPts);
	
		for ( int i = 0; i < nPts; i++ ) {
			Vector3f point = v.elementAt(i);
			int x = (int)point.X;
			int y = (int)point.Y;
			Vector2f in = new Vector2f(point.X, point.Y);
			Vector2f out = new Vector2f(0, 0);
			MipavCoordinateSystems.CartesianToPolar2D(in, out, srcImage);
			r = out.X;
			theta = out.Y;
			voiTable.insert(x, y, r, theta);
		}		
		
	}

	
}
/*
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
*/