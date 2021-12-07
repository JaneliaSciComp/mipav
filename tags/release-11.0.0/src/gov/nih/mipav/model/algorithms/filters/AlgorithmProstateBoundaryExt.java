package gov.nih.mipav.model.algorithms.filters;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
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
import java.awt.Point;

import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibFoundation.Curves.*;

/**
 * This algorithm traces the prostate boundary from the target image, non prostate binary mask image and 
 * prostate boundary binary mask image.    After AAM generates the initial contour, the contour is copied
 * to all the three images.    Target image represent the test image original intensity texture.   non-prostate
 * binary image and prostate boundary image are generated from SVM classification.  
 * 
 * Current tracing mechanism is polar coordinate based, along the AAM generated initial contour landmark points, 
 * the alpha angel ranges from 0 to 360 degree, the alpha expansion line is used for tracing the ideal prostate 
 * boundary point.   A basic voting mechanism is applied to find the ideal point. 
 * 
 * Lately, we will use normal line of the contour curve from the landmark point to represent the tracing path.     
 * 
 * @author Ruida Cheng
 *
 */
public class AlgorithmProstateBoundaryExt extends AlgorithmBase implements
		AlgorithmInterface {

	/** result VOI */
	private VOI resultVOI;

	/** target image */
	public ModelImage srcImage;

	/** VOIs  */
	private VOIVector VOIs;

	/** non-prostate image */
	private ModelImage nonProstateImage;
	
	/** boundary image. */
	private ModelImage boundaryImage;

	/** final VOI */ 
	private VOIVector expandVOI;

	/**
	 * Constructor
	 * @param _srcImage          target image
	 * @param _nonProstateImage		non-prostate binary mask image
	 * @param _boundaryImage		prostate boundary mask image. 
	 * @param _imageSliceNumber	    image slice number, trivial. 
	 */
	public AlgorithmProstateBoundaryExt(ModelImage _srcImage,
			ModelImage _nonProstateImage, ModelImage _boundaryImage,
			int _imageSliceNumber) {
		srcImage = _srcImage;
		nonProstateImage = _nonProstateImage;
		nonProstateImage.addVOIs(srcImage.getVOIs());
		boundaryImage = _boundaryImage;
		boundaryImage.addVOIs(srcImage.getVOIs());

	}

	public void algorithmPerformed(AlgorithmBase algorithm) {
	}

	/**
	 * Prepares this class for destruction.
	 */
	public void finalize() {

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

		smoothVOI30(srcImage, srcImage);
		findBoundaryNarrowBand();

	}

	/**
	 * Smooth the AAM initial generated contour with 30 landmark points. 
	 * @param maskImage      binary mask image
	 * @param resultImage		result image 
	 */
	public void smoothVOI30(ModelImage maskImage, ModelImage resultImage) {

		VOIVector v = maskImage.getVOIs();
		if (v.size() == 0)
			return;
		v.VOIAt(0).setActive(true);
		v.VOIAt(0).setAllActive(true);

		try {
			AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(maskImage,
					v.VOIAt(0), 30, false);
			smoothAlgo.addListener(this);
			smoothAlgo.run();

			VOIVector resultVOIs = resultImage.getVOIs();
			VOI resultVOI = smoothAlgo.getResultVOI();
			resultVOIs.VOIAt(0).removeCurves();
			resultVOIs.VOIAt(0).setCurves(resultVOI.getCurves());
			resultImage.notifyImageDisplayListeners(null, true);
			smoothAlgo.setCompleted(true);
			smoothAlgo.finalize();
			smoothAlgo = null;
		} catch (OutOfMemoryError x) {
			MipavUtil
					.displayError("Dialog Smooth: unable to allocate enough memory");

			return;
		}
	}

	/**
	 * Voting based tracing for the final prostate boundary. 
	 */
	public void findBoundaryNarrowBand() {

		int zDim = 1;
		int x, y;
		float r, theta;
		float best_r, best_theta;
		double highest_intensity;
		int i, j;

		if (srcImage.getVOIs() != null) {
			this.VOIs = srcImage.getVOIs();
		}

		// find the intersection of the lower bound with the VOI.
		Vector<VOIBase>[] vArray = VOIs.VOIAt(0).getSortedCurves(VOIBase.ZPLANE, zDim);
		VOIBase v = vArray[0].get(0);

		int nPts = v.size();

		Vector<Point> extractedPoints = new Vector<Point>();

		// Construct the VOI contour's approximate points
		float mid_r;
		for (i = 0; i < nPts; i++) {

			x = (int) ((Vector3f) (v.elementAt(i))).X;
			y = (int) ((Vector3f) (v.elementAt(i))).Y;

			Vector2f in = new Vector2f(x, y);
			Vector2f out = new Vector2f(0, 0);

			MipavCoordinateSystems.CartesianToPolar2D(in, out, srcImage);

			r = out.X;
			theta = out.Y;

			best_r = r;
			best_theta = theta;
			highest_intensity = 0;

			Hashtable<Integer, PointAttribute> hashIntensityUp = new Hashtable<Integer, PointAttribute>();
			Hashtable<Integer, PointAttribute> hashIntensityNonUp = new Hashtable<Integer, PointAttribute>();
			Hashtable<Integer, PointAttribute> hashIntensityBoundaryUp = new Hashtable<Integer, PointAttribute>();

			Hashtable<Integer, PointAttribute> hashIntensityDown = new Hashtable<Integer, PointAttribute>();
			Hashtable<Integer, PointAttribute> hashIntensityNonDown = new Hashtable<Integer, PointAttribute>();
			Hashtable<Integer, PointAttribute> hashIntensityBoundaryDown = new Hashtable<Integer, PointAttribute>();

			// looking down for the boundary
			mid_r = r;
			for (j = 0; j < 4; j++) {
				in = new Vector2f(r, theta);
				out = new Vector2f(0, 0);
				MipavCoordinateSystems.PolarToCartesian2D(in, out, srcImage);
				x = (int) out.X;
				y = (int) out.Y;

				// intensity
				double intensity;
				double intensity_non;
				double intensity_b;

				int xDim = srcImage.getExtents()[0];
				int yDim = srcImage.getExtents()[1];

				if (x < 0 || x > (xDim - 1) || y < 0 || y > (yDim - 1))
					continue;

				intensity = srcImage.getDouble(x, y);
				hashIntensityDown.put(j,
						new PointAttribute(r, theta, intensity));

				intensity_non = nonProstateImage.getDouble(x, y);
				hashIntensityNonDown.put(j, new PointAttribute(r, theta, intensity_non));

				intensity_b = boundaryImage.getDouble(x, y);
				hashIntensityBoundaryDown.put(j, new PointAttribute(r, theta, intensity_b));

				r = r - 2f;
			} // end for loop j

			for (int k = 1; k < hashIntensityDown.size(); k++) {
				PointAttribute prev = hashIntensityDown.get(k - 1);
				PointAttribute current = hashIntensityDown.get(k);

				PointAttribute prev_non = hashIntensityNonDown.get(k - 1);
				PointAttribute current_non = hashIntensityNonDown.get(k);

				PointAttribute prev_boundary = hashIntensityBoundaryDown.get(k - 1);
				PointAttribute current_boundary = hashIntensityBoundaryDown.get(k);

				if ((prev_non.intensity == 0 && current_non.intensity == 0)
						&& (prev_boundary.intensity == 1 && current_boundary.intensity == 1)) {
					// move on
					continue;
				} else if ((prev_non.intensity == 1 && current_non.intensity == 1)
						&& (prev_boundary.intensity == 0 && current_boundary.intensity == 0)) {
					// move on
					continue;

				} else if (Math.abs(current.intensity - prev.intensity) > highest_intensity) {
					
					if ((Math.abs(current_non.intensity - prev_non.intensity) > 0 && Math
							.abs(current_boundary.intensity
									- prev_boundary.intensity) > 0)) {
						// if both non_prostate and prostate boundary exist,
						// stop search
						highest_intensity = Math.abs(current.intensity
								- prev.intensity);
						best_r = current.r;
						best_theta = current.theta;

						break;
					}

				}
			}

			// looking up for the boundary
			r = mid_r;
			// mid_r = r;
			for (j = 0; j < 8; j++) {
				in = new Vector2f(r, theta);
				out = new Vector2f(0, 0);
				MipavCoordinateSystems.PolarToCartesian2D(in, out, srcImage);
				x = (int) out.X;
				y = (int) out.Y;

				// intensity
				double intensity;
				double intensity_non;
				double intensity_b;

				int xDim = srcImage.getExtents()[0];
				int yDim = srcImage.getExtents()[1];

				if (x < 0 || x > (xDim - 1) || y < 0 || y > (yDim - 1))
					continue;

				intensity = srcImage.getDouble(x, y);
				hashIntensityUp.put(j, new PointAttribute(r, theta, intensity));

				intensity_non = nonProstateImage.getDouble(x, y);
				hashIntensityNonUp.put(j, new PointAttribute(r, theta,
						intensity_non));

				intensity_b = boundaryImage.getDouble(x, y);
				hashIntensityBoundaryUp.put(j, new PointAttribute(r, theta,
						intensity_b));

				r = r + 2f;
			} // end for loop j

			for (int k = 1; k < hashIntensityUp.size(); k++) {
				PointAttribute prev = hashIntensityUp.get(k - 1);
				PointAttribute current = hashIntensityUp.get(k);

				PointAttribute prev_non = hashIntensityNonUp.get(k - 1);
				PointAttribute current_non = hashIntensityNonUp.get(k);

				PointAttribute prev_boundary = hashIntensityBoundaryUp
						.get(k - 1);
				PointAttribute current_boundary = hashIntensityBoundaryUp
						.get(k);

				if ((prev_non.intensity == 0 && current_non.intensity == 0)
						&& (prev_boundary.intensity == 1 && current_boundary.intensity == 1)) {
					// move on
					if (r <= mid_r + 2) {
						if (Math.abs(current.intensity - prev.intensity) > highest_intensity) {
							best_r = current.r;
							best_theta = current.theta;
							highest_intensity = Math.abs(current.intensity
									- prev.intensity);
						}
					}
					continue;
				} else if ((prev_non.intensity == 1 && current_non.intensity == 1)
						&& (prev_boundary.intensity == 0 && current_boundary.intensity == 0)) {
					// move on

					if (r <= mid_r + 2) {
						if (Math.abs(current.intensity - prev.intensity) > highest_intensity) {
							best_r = current.r;
							best_theta = current.theta;
							highest_intensity = Math.abs(current.intensity
									- prev.intensity);
						}
					}
					continue;

				} else if (Math.abs(current.intensity - prev.intensity) > highest_intensity) {

					highest_intensity = Math.abs(current.intensity
							- prev.intensity);
					best_r = current.r;
					best_theta = current.theta;

					if ((Math.abs(current_non.intensity - prev_non.intensity) > 0 && Math
							.abs(current_boundary.intensity
									- prev_boundary.intensity) > 0)) {
						// if both non_prostate and prostate boundary exist,
						// stop search
						highest_intensity = Math.abs(current.intensity
								- prev.intensity);
						best_r = current.r;
						best_theta = current.theta;

						break;
					}
				}
			}

			hashIntensityUp = null;
			hashIntensityNonUp = null;
			hashIntensityBoundaryUp = null;

			hashIntensityDown = null;
			hashIntensityNonDown = null;
			hashIntensityBoundaryDown = null;

			// B-spline transform
			in = new Vector2f(best_r, best_theta);
			out = new Vector2f(0, 0);

			MipavCoordinateSystems.PolarToCartesian2D(in, out, srcImage);
			x = (int) out.X;
			y = (int) out.Y;

			extractedPoints.add(new Point(x, y));

		} // end for loop i < nPts

		nPts = extractedPoints.size();
		Vector3f[] BsplinePoints = new Vector3f[nPts];
		for (int w = 0; w < nPts; w++) {
			Point p = extractedPoints.get(w);
			BsplinePoints[w] = new Vector3f(p.x, p.y, 0);
		}

		BSplineCurve3f curve = BSplineCurve3f.CreateApproximation(
				BsplinePoints, nPts - 5, 2);
		float minTime = curve.GetMinTime();
		float maxTime = curve.GetMaxTime();
		// float step = (maxTime - minTime) / 30f;
		float step = (maxTime - minTime) / 15f;

		extractedPoints.clear();
		for (float t = minTime; t <= maxTime; t += step) {
			Vector3f pt = curve.GetPosition(t);
			extractedPoints.add(new Point((int) pt.X, (int) pt.Y));
		}

		VOIBase vTemp = (VOIBase) v.clone();
		nPts = extractedPoints.size();

		// zero out the z dimension VOI
		float[] xPts = new float[nPts];
		float[] yPts = new float[nPts];
		float[] zPtsZero = new float[nPts];

		for (int u = 0; u < nPts; u++) {
			Point p = extractedPoints.get(u);
			xPts[u] = p.x;
			yPts[u] = p.y;
		}

		vTemp.importArrays(xPts, yPts, zPtsZero, nPts);

		expandVOI = new VOIVector();
		VOI voiNew = new VOI((short) 0, "blank");
		voiNew.importCurve(vTemp);
		expandVOI.add(voiNew);

	}

	/**
	 * get the final VOI
	 * @return   final prostate VOI
	 */
	public VOIVector getExtractedVOI() {
		return expandVOI;
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

	class PointAttribute {
		double gradient_mag_src;
		double gradient_dir_src;
		double gradient_mag_non;
		double gradient_dir_non;
		double gradient_mag_b;
		double gradient_dir_b;
		double intensity;
		int x;
		int y;
		float r;
		float theta;

		public PointAttribute(int _x, int _y, double _gradient_mag_src,
				double _gradient_dir_src, double _gradient_mag_non,
				double _gradient_dir_non, double _gradient_mag_b,
				double _gradient_dir_b) {
			x = _x;
			y = _y;
			gradient_mag_src = _gradient_mag_src;
			gradient_dir_src = _gradient_dir_src;
			gradient_mag_non = _gradient_mag_non;
			gradient_dir_non = _gradient_dir_non;
			gradient_mag_b = _gradient_mag_b;
			gradient_dir_b = _gradient_dir_b;

		}

		public PointAttribute(float _r, float _theta, double _intensity) {
			r = _r;
			theta = _theta;
			intensity = _intensity;
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

}