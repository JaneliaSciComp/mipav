package gov.nih.mipav;

import gov.nih.mipav.model.structures.*;

import WildMagic.LibFoundation.Mathematics.*;

public class MIPAVPolarCoordinateSystems {

	/* Clock wise traversal.  In general, always counter closewise.  */
	private static boolean clockWise = false;

	/**
	 * Convert the Cartesian coordinate into the Polar coordinate, point based. 
	 * 
	 * @param in     image pixel coordinate, in.x = x, in.y = y.
	 * @param out    polar coordinate  out.x = r, out.y = theta. 
	 * @param image  source image. 
	 */
	public static final void CartesianToPolar2D(Vector2f in, Vector2f out, ModelImage image) {
		// get the Pixel coordinate center
		Vector3f center = image.getImageCenter();

		float centerX = center.X;
		float centerY = center.Y;

		float xx = in.X;
		float yy = in.Y;
		
		// Convert the pixel coordinate to Cartesian coordinate. 
		float x = xx - centerX;
		float y = centerY - yy;
		
		// calculate the Polar coordinate  
		float r = getRadius(x, y);
		float angle = getAngleInDegree(x, y);
		
		out.X = r;
		out.Y = angle;
	}
	
	/**
	 * Convert the Cartesian coordinate into the Polar coordinate.
	 * 
	 * @param image       source model image.
	 * @param imageR      image R buffer to save the r value of Polar coordinate.
	 * @param imageTheta  image Theta buffer to save the theta value of the Polar coordinate. 
	 */
	public static final void CartesianToPolar2D(ModelImage image,
			ModelSimpleImage imageR, ModelSimpleImage imageTheta) {

		// get the Pixel coordinate center
		Vector3f center = image.getImageCenter();

		float centerX = center.X;
		float centerY = center.Y;

		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];

		for (int yy = 0; yy < yDim; yy++) {
			for (int xx = 0; xx < xDim; xx++) {
				// Convert the pixel coordinate to Cartesian coordinate. 
				float x = xx - centerX;
				float y = centerY - yy;

				// calculate the Polar coordinate  
				float r = getRadius(x, y);
				float angle = getAngleInDegree(x, y);

				// save the R, angle values in the R and Theta images. 
				imageR.setValue(xx, yy, r);
				imageTheta.setValue(xx, yy, angle);

			}
		}
	}

	/**
	 * Convert the Polar coordinate to Cartesian coordinate, point based. 
	 * @param in     polar coordinate,  in.x = r, in.y = theta.
	 * @param out    image pixel coordinate, out.x = x, out.y = y;
	 * @param image  source image. 
	 */
	public static void PolarToCartesian2D(Vector2f in, Vector2f out, ModelImage image) {
		// get the center of Cartesian space
		Vector3f center = image.getImageCenter();

		float centerX = center.X;
		float centerY = center.Y;
		
		float r = in.X;
		float angle = (float)((in.Y / (float) 360) * Math.PI * 2);

		// -- Need convert (x,y) into pixel coordinates
		int x = (int)(getCartesianX(r, angle) + centerX);
		int y = (int)(centerY - getCartesianY(r, angle));

		out.X = x;
		out.Y = y;
	}
	
    /**
     * Whether this routine is necessary, the question will to be decided in the future.
     * This approach just converts x, y Cartesian from polar coordinate perspective. 
     * Actually, the imageR and imageTheta should be the input images, and the source image should hold
     * the Cartesian coordinate results.   We need to decide this later.   
	 * Reference: imageJ Polar coordinate transformer. 
	 * 
     * @param image       source image
     * @param imageR      polar coordinate R image. 
     * @param imageTheta  polar coordinate Theta image. 
     */
	public static void PolarToCartisian2D(ModelImage image, ModelSimpleImage imageR, ModelSimpleImage imageTheta) {

		// get the center of Cartesian space
		Vector3f center = image.getImageCenter();

		float centerX = center.X;
		float centerY = center.Y;

		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];

		// longest radius from the center of image to the 4 corners.
		double radius_longest;

		// Set up the Polar Grid:
		// Use y values for angles
		// -- Need 360 degrees (0 to 359...)

		// Line width will be:
		// -- equal to radius -- Need to find the greatest radius
		// -- (4 possibilities: from center to each corner)
		// -- Top-Left Corner (0,0):
		double radius = Math.sqrt((centerX - 0) * (centerX - 0) + (centerY - 0)
				* (centerY - 0));
		// -- Top-Right Corner (xDim, 0):
		double radiusTemp = Math.sqrt((xDim - centerX) * (xDim - centerX)
				+ (centerY - 0) * (centerY - 0));
		if (radiusTemp > radius)
			radius = radiusTemp;
		// -- Bottom-Left Corner (0, yDim):
		radiusTemp = Math.sqrt((centerX - 0) * (centerX - 0) + (yDim - centerY)
				* (yDim - centerY));
		if (radiusTemp > radius)
			radius = radiusTemp;
		// -- Bottom-Right Corner (xDim , yDim):
		radiusTemp = Math.sqrt((centerX - xDim) * (centerX - xDim)
				+ (centerY - yDim) * (centerY - yDim));
		if (radiusTemp > radius)
			radius = radiusTemp;
		int radiusInt = (int) radius;
		radius_longest = radiusInt;

		// Fill the Polar Grid
		for (int yy = 0; yy < 360; yy++) {
			for (int xx = 0; xx < radius_longest; xx++) {

				// For each polar pixel, need to convert it to Cartesian
				// coordinates
				float r = xx;
				float angle = (float)((yy / (float) 360) * Math.PI * 2);

				// -- Need convert (x,y) into pixel coordinates
				int x = (int)(getCartesianX(r, angle) + centerX);
				int y = (int)(centerY - getCartesianY(r, angle));

				imageR.setValue(x, y, r);
				imageTheta.setValue(x, y, angle);
				
			}
		}

	}

	/**
	 * Givem Polar coordinate system r and theta, calculate the Cartesian coordinate X. 
	 * @param r      polar coordinate r value.
	 * @param angle  poloar coordinate theta value.
	 * @return  Cartesian coordinate X value.  
	 */
	private static float getCartesianX(float r, float angle) {
		return (float)(r * Math.cos(angle));
	}

	/**
	 * Givem Polar coordinate system r and theta, calculate the Cartesian coordinate Y. 
	 * @param r      polar coordinate r value.
	 * @param angle  poloar coordinate theta value.
	 * @return  Cartesian coordinate Y value.  
	 */
	
	private static float getCartesianY(float r, float angle) {
		float y = (float)(r * Math.sin(angle));
		return clockWise ? -y : y;
	}

	/**
	 * Get the radius for the polar coordinate from the given Cartesian coordinate
	 * @param x  Cartesian x coordinate
	 * @param y  Cartesian y coordinate
	 * @return  polar coordinate radius 
	 */
	private static float getRadius(float x, float y) {
		return (float) Math.sqrt(x * x + y * y);
	}

	/**
	 * Get the polar theta angle in Degree [0, 360].
	 * Reference: imageJ polar coordinate transformer. 
	 * 
	 * @param x  Cartesian x coordinate
	 * @param y  Cartesian y coordinate
	 * @return  the polar theta angle in degree. 
	 */
	private static float getAngleInDegree(float x, float y) {

		// Returns an angle in the range [0, 360]
		float angle = (float) Math.toDegrees(Math.atan2(y, x));
		if (angle < 0) {
			angle += 360;
		}
		return clockWise ? 360f - angle : angle;
	}

	/***
	 * Get the polar theta value in radians [0 , 2*PI].  
	 * Reference:  http://en.wikipedia.org/wiki/Polar_coordinate_system.
	 * 
	 * @param x  Cartesian x coordinate
	 * @param y  Cartesian y coordinate
	 * @return  the polar theta angle in radians. 
	 */
	private static float getAngleInRadians(float x, float y) {
		if (x > 0 && y >= 0) {
			return (float) Math.atan2(y, x);
		} else if (x > 0 && y < 0) {
			return (float) (Math.atan2(y, x) + 2 * Math.PI);
		} else if (x < 0) {
			return (float) (Math.atan2(y, x) + Math.PI);
		} else if (x == 0 && y > 0) {
			return (float) (0.5f * Math.PI);
		} else if (x == 0 && y < 0) {
			return (float) (1.5f * Math.PI);
		} else { // x = 0 && y = 0
			return 0;
		}
	}
}