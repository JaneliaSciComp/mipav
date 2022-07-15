package WildMagic.ApplicationDemos;

import java.awt.Color;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Vector;

import WildMagic.LibFoundation.Curves.NaturalSpline3;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.filters.OpenCL.filters.OpenCLAlgorithmGaussianBlur;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogBase;

public class untwistTest
{
	private String directory = "C:\\mipav\\worm-demo\\";

	private Vector<Vector3f> left = null;
	private Vector<Vector3f> right = null;
	private ModelImage image = null;

	/**
	 * @param args
	 */
	public static void main(String[] args) {            
		final ViewUserInterface ui = ViewUserInterface.create();

		new untwistTest();
	}

	public untwistTest()
	{

		File file = new File(directory);
		if ( file.exists() && file.isDirectory() ) {
			final String[] list = file.list();
			for (int i = 0; i < list.length; i++) {
				System.err.println(list[i]);
				if ( list[i].equals("lattice.csv") ) {
					openLattice(list[i]);
				}
				else if ( list[i].endsWith(".tif")) {
					openImage(list[i]);
				}
			}
		}
		if ( image != null && left != null && right != null && 
				left.size() == right.size() && left.size() > 0 ) {
			// use the lattice to untwist the image and write the output
			untwist();
		}

		System.exit(0);
	}

	private void openLattice( String filename ) {
		File file = new File(directory + filename);
		if ( file.exists() )
		{		
			FileReader fr;
			try {
				fr = new FileReader(file);
				BufferedReader br = new BufferedReader(fr);
				String line = br.readLine();
				line = br.readLine();

				left = new Vector<Vector3f>();
				right = new Vector<Vector3f>();

				while ( line != null && (line.length() > 1) )
				{
					String[] parsed = line.split( "," );
					if ( parsed.length != 0 )
					{
						int parsedIndex = 0;
						String name = String.valueOf( parsed[parsedIndex++] );
						float x    = (parsed.length > parsedIndex+0) ? (parsed[parsedIndex+0].length() > 0) ? Float.valueOf( parsed[parsedIndex+0] ) : 0 : 0; 
						float y    = (parsed.length > parsedIndex+1) ? (parsed[parsedIndex+1].length() > 0) ? Float.valueOf( parsed[parsedIndex+1] ) : 0 : 0; 
						float z    = (parsed.length > parsedIndex+2) ? (parsed[parsedIndex+2].length() > 0) ? Float.valueOf( parsed[parsedIndex+2] ) : 0 : 0;
						left.add( new Vector3f(x,y,z) );
					}
					line = br.readLine();
					parsed = line.split( "," );
					if ( parsed.length != 0 )
					{
						int parsedIndex = 0;
						String name = String.valueOf( parsed[parsedIndex++] );
						float x    = (parsed.length > parsedIndex+0) ? (parsed[parsedIndex+0].length() > 0) ? Float.valueOf( parsed[parsedIndex+0] ) : 0 : 0; 
						float y    = (parsed.length > parsedIndex+1) ? (parsed[parsedIndex+1].length() > 0) ? Float.valueOf( parsed[parsedIndex+1] ) : 0 : 0; 
						float z    = (parsed.length > parsedIndex+2) ? (parsed[parsedIndex+2].length() > 0) ? Float.valueOf( parsed[parsedIndex+2] ) : 0 : 0;

						right.add( new Vector3f(x,y,z) );
					}
					line = br.readLine();
				}
				fr.close();
			} catch (FileNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	private void openImage( String filename ) {
		FileIO fileIO = new FileIO();
		image = fileIO.readImage(filename, directory, false, null);
		image.calcMinMax();
		image.setResolutions(new float[] { 1, 1, 1 });
	}

	private void untwist() {
		// This is a simplified version of the algorithm written to demonstrate untwisting
		// The left and right lists represents pairs points along the left-edge and right-edges of the 
		// embroynic worm. The pairs of points are used to define the outline of the worm from
		// head to tail, enabling the alogrithm to constuct a 3D curve that represents the
		// midline of the worm as well as a set of basis vectors that define 2D planes at
		// each point along the midline curve. The 2D planes are used to resample the volume
		// and reconstruct a 'straightened' version of the curved worm

		// 1). Define the control points of the center spline curve:
		Vector3f[] centerControlPts = new Vector3f[left.size()];
		for ( int i = 0; i < left.size(); i++ ) {
			// calculate the mid-point between the left and right pair:
			Vector3f pt = Vector3f.add(left.elementAt(i), right.elementAt(i) );
			pt.scale(0.5f);
			// add the mid-point to the center control points:
			centerControlPts[i] = pt;
		}

		// 2). Generate a Natural Spline curve that passes through all midpoints:
		float totalDistance = 0;
		for (int i = 0; i < centerControlPts.length - 1; i++) {
			totalDistance += centerControlPts[i].distance(centerControlPts[i + 1]);
		}
		float[] time = new float[centerControlPts.length]; 
		time[0] = 0;
		float distance = 0;
		for (int i = 1; i < centerControlPts.length; i++) {
			distance += centerControlPts[i].distance(centerControlPts[i - 1]);
			time[i] = distance / totalDistance;
		}
		NaturalSpline3 centerSpline = new NaturalSpline3(NaturalSpline3.BoundaryType.BT_FREE, 
				centerControlPts.length - 1, time, centerControlPts);

		// 3). Reparameterize the spline so the length is divided into even steps:
		float length = centerSpline.GetLength(0, 1);
		int maxLength = (int)Math.ceil(length);
		float step = 1;
		if ( maxLength != length )
		{
			step = (length / maxLength);
		}
		time = new float[maxLength + 1];
		Vector<Vector3f> centerCurve = new Vector<Vector3f>();
		for (int i = 0; i <= maxLength; i++) {
			final float t = centerSpline.GetTime(i*step);
			centerCurve.add(centerSpline.GetPosition(t));
			time[i] = t;
		}

		// 4). Generate a set of basis vectors that define a set of 2D planes 
		// for each voxel along the centerSpline. The point on the spline curve is the point on the plane,
		// the plane normal is the first derivative at that point, the right axis is the vector from the center to the
		// corresponding point along the right side of the worm and the up axis is the cross product
		// of the tangent and the right axis.

		// 4a) Because the left and right pairs of points along the worm are widely spaced we must
		// interpolate the right axis vector to maintain a smooth boundary edge.
		int[] interpolationIndex = new int[centerControlPts.length];
		interpolationIndex[0] = 0; // start of centerCurve
		interpolationIndex[interpolationIndex.length - 1] = maxLength; // end of centerCurve
		// determine which contol points to interpolate between for the right edge of the worm:
		for ( int i = 1; i < interpolationIndex.length; i++ )
		{
			interpolationIndex[i] = -1;
			int minIndex = -1;
			float minDist = Float.MAX_VALUE;
			for ( int j = 0; j <= maxLength; j++ )
			{	
				// find closest spline point to this center:
				Vector3f centerPt = centerCurve.elementAt(j);
				if ( centerPt.isEqual(centerControlPts[i]) )
				{
					interpolationIndex[i] = j;
					break;
				}
				distance = centerPt.distance(centerControlPts[i] );
				if ( distance < minDist )
				{
					minDist = distance;
					minIndex = j;
				}
			}
			if ( (interpolationIndex[i] == -1) && (minIndex != -1 ))
			{
				interpolationIndex[i] = minIndex;
			}
		}
		// interpolate the vector to the right edge of the worm between control points
		// the worm diameter is interpolated as well.
		float[] diameters = new float[maxLength+1];
		Vector3f[] rightVectorsInterp = new Vector3f[maxLength+1];
		for ( int i = 0; i < centerControlPts.length - 1; i++ )
		{
			int startIndex = interpolationIndex[i];
			int endIndex = interpolationIndex[i+1];
			float startRadius = (left.elementAt(i).distance(right.elementAt(i)))/2f;
			float endRadius = (left.elementAt(i+1).distance(right.elementAt(i+1)))/2f;

			Vector3f startRight = Vector3f.sub( right.elementAt(i), left.elementAt(i) );
			startRight.normalize();
			Vector3f endRight = Vector3f.sub( right.elementAt(i+1), left.elementAt(i+1) );
			endRight.normalize();

			Vector3f rotationAxis = Vector3f.cross(startRight, endRight);
			rotationAxis.normalize();
			float angle = startRight.angle(endRight);
			int steps = endIndex - startIndex;
			float fAngle = angle / steps;
			Matrix3f mat = new Matrix3f(true);
			mat.fromAxisAngle(rotationAxis, angle);
			for ( int j = startIndex; j <= endIndex; j++ )
			{
				float interp = (j - startIndex) / (float)(endIndex - startIndex);
				diameters[j] = (1 - interp) * startRadius + interp * endRadius;

				mat.fromAxisAngle(rotationAxis, (j - startIndex)*fAngle);
				rightVectorsInterp[j] = mat.multRight(startRight);
				rightVectorsInterp[j].normalize();
			}
			float diffAngle = rightVectorsInterp[endIndex].angle(endRight);
			if ( (diffAngle/Math.PI)*180 > 2 )
			{
				angle = (float) ((2*Math.PI) - angle); 				
				fAngle = angle / steps;
				for ( int j = startIndex; j <= endIndex; j++ )
				{					
					mat.fromAxisAngle(rotationAxis, (j - startIndex)*fAngle);
					rightVectorsInterp[j] = mat.multRight(startRight);
					rightVectorsInterp[j].normalize();
				}
			}
		}
		int extent = 0;
		for (int i = 0; i <= maxLength; i++) {
			if (diameters[i] > extent) {
				extent = (int) Math.ceil(diameters[i]);
			}
		}
		// 4b) Use the interpolated right edge and diameters to create the basis vectors for the planes:
		extent += 30;
//		System.err.println("generating planes " + extent);
		Vector3f[][] planes = new Vector3f[maxLength+1][];
		for (int i = 0; i <= maxLength; i++) {
			final float t = time[i];			
			final Vector3f point = centerCurve.elementAt(i);
			Vector3f normal = centerSpline.GetTangent(t);
			normal.normalize();
			final Vector3f rightVector = new Vector3f(rightVectorsInterp[i]);	
			float diameter = diameters[i];

			final Vector3f upVector = Vector3f.cross(normal, rightVector);
			upVector.normalize();

			rightVector.scale(extent);
			upVector.scale(extent);

			planes[i] = new Vector3f[4];
			planes[i][0] = Vector3f.add(Vector3f.neg(rightVector), Vector3f.neg(upVector));
			planes[i][1] = Vector3f.add(rightVector, Vector3f.neg(upVector));
			planes[i][2] = Vector3f.add(rightVector, upVector);
			planes[i][3] = Vector3f.add(Vector3f.neg(rightVector), upVector);
			for (int j = 0; j < 4; j++) {
				planes[i][j].add(point);
			}
		}

		// 5) For each plane scan the 3D Volume from left to right, top to bottom, writing the voxels into the
		// corresponding pixels in the plane. This produces the 'straightened' image of the worm.
		int[] resultExtents = new int[]{ 2 * extent, 2 * extent, centerCurve.size() };
		String newName = JDialogBase.makeImageName(image.getImageFileName(), "_straight_unmasked.tif");
		ModelImage resultImage = new ModelImage(ModelStorageBase.FLOAT, resultExtents, newName);

		for ( int i = 0; i < centerCurve.size(); i++ ) {
			writeDiagonal(image, resultImage, 0, i, resultExtents, planes[i]);
		}
		// save to output directory
		directory += "output" + File.separator;
		resultImage.setImageDirectory(newName);
		ModelImage.saveImage(resultImage, newName, directory, false);

		//6). The same ellipses are used to mask the straightened worm so places where the planes overlap
		// sections of the worm outside the current worm boundary are removed.
		ModelImage contourImage = new ModelImage( ModelStorageBase.FLOAT, resultImage.getExtents(), 
				JDialogBase.makeImageName(image.getImageFileName(), "_straight_contour.xml") );
		contourImage.setResolutions( resultImage.getResolutions(0) );

		int dimX = (resultImage.getExtents()[0]);
		int dimY = (resultImage.getExtents()[1]);
		int dimZ = (resultImage.getExtents()[2]);
		Vector3f center = new Vector3f( dimX/2, dimY/2, 0 );
		float maxDist = Vector3f.normalize(center);

		for (int i = 0; i < dimZ; i++)
		{			
			float radius = diameters[i];
			radius += 5; // default padding factor		

			float radiusSq = radius*radius;
			for ( int y = (int)Math.max(0, center.Y - radius); y < Math.min(dimY, center.Y + radius + 1); y++ )
			{
				for ( int x = (int)Math.max(0, center.X - radius); x < Math.min(dimX, center.X + radius + 1); x++ )
				{
					float dist = (x - center.X) * (x - center.X) + (y - center.Y) * (y - center.Y); 
					if ( dist <= radiusSq )
					{
						contourImage.set(x,  y, i, 10 );
					}
				}
			}
		}

		// Optional VOI interpolation & smoothing:
		ModelImage contourImageBlur = blur(contourImage, 3);
		contourImage.disposeLocal(false);
		contourImage = null;

		for (int z = 0; z < dimZ; z++)
		{			
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					if ( contourImageBlur.getFloat(x,y,z) <= 1 )
					{
						if ( resultImage.isColorImage() )
						{
							resultImage.setC(x, y, z, 0, 0);	
							resultImage.setC(x, y, z, 1, 0);	
							resultImage.setC(x, y, z, 2, 0);	
							resultImage.setC(x, y, z, 3, 0);							
						}
						else
						{
							resultImage.set(x, y, z, 0);
						}
					}
				}
			}			
		}

		newName = JDialogBase.makeImageName(image.getImageFileName(), "_straight.tif");
		resultImage.setImageName( newName );
		ModelImage.saveImage(resultImage, newName, directory, false);
	}

	protected void writeDiagonal(final ModelImage image, final ModelImage result, final int tSlice, final int slice,
			final int[] extents, final Vector3f[] verts) {
		final int iBound = extents[0];
		final int jBound = extents[1];
		final int[] dimExtents = image.getExtents();

		/*
		 * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
		 * coordinate-systems: transformation:
		 */
		final int iFactor = 1;
		final int jFactor = dimExtents[0];
		final int kFactor = dimExtents[0] * dimExtents[1];
		final int tFactor = dimExtents[0] * dimExtents[1] * dimExtents[2];

		int buffFactor = 1;

		if ( (image.getType() == ModelStorageBase.ARGB) || (image.getType() == ModelStorageBase.ARGB_USHORT)
				|| (image.getType() == ModelStorageBase.ARGB_FLOAT)) {
			buffFactor = 4;
		}

		final Vector3f center = new Vector3f();
		for (int i = 0; i < verts.length; i++) {
			center.add(verts[i]);
		}
		center.scale(1f / verts.length);

		/* Calculate the slopes for traversing the data in x,y,z: */
		float xSlopeX = verts[1].X - verts[0].X;
		float ySlopeX = verts[1].Y - verts[0].Y;
		float zSlopeX = verts[1].Z - verts[0].Z;

		float xSlopeY = verts[3].X - verts[0].X;
		float ySlopeY = verts[3].Y - verts[0].Y;
		float zSlopeY = verts[3].Z - verts[0].Z;

		float x0 = verts[0].X;
		float y0 = verts[0].Y;
		float z0 = verts[0].Z;

		xSlopeX /= (iBound);
		ySlopeX /= (iBound);
		zSlopeX /= (iBound);

		xSlopeY /= (jBound);
		ySlopeY /= (jBound);
		zSlopeY /= (jBound);

		/* loop over the 2D image (values) we're writing into */
		float x = x0;
		float y = y0;
		float z = z0;

		for (int j = 0; j < jBound; j++) {

			/* Initialize the first diagonal point(x,y,z): */
			x = x0;
			y = y0;
			z = z0;

			for (int i = 0; i < iBound; i++) {
				// Initialize to 0:
				if (buffFactor == 4) {						
					result.setC(i, j, slice, 0, 0 );
					result.setC(i, j, slice, 1, 0 );
					result.setC(i, j, slice, 2, 0 );
					result.setC(i, j, slice, 3, 0 );
				}
				else {
					result.set(i, j, slice, 0 );
				}		

				final int iIndex = Math.round(x);
				final int jIndex = Math.round(y);
				final int kIndex = Math.round(z);

				/* calculate the ModelImage space index: */
				final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));

				// Bounds checking:
				if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
						|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > image.getSize()))) {

					// do nothing
				} else {
					/* if color: */
					if (buffFactor == 4) {						
						result.setC(i, j, slice, 0, image.getFloatC(iIndex, jIndex, kIndex, 0) );
						result.setC(i, j, slice, 1, image.getFloatC(iIndex, jIndex, kIndex, 1) );
						result.setC(i, j, slice, 2, image.getFloatC(iIndex, jIndex, kIndex, 2) );
						result.setC(i, j, slice, 3, image.getFloatC(iIndex, jIndex, kIndex, 3) );
					}
					/* not color: */
					else {
						result.set(i, j, slice, image.getFloat(iIndex, jIndex, kIndex));
					}
				}

				/*
				 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
				 * ySlopeX and zSlopeX values:
				 */
				x = x + xSlopeX;
				y = y + ySlopeX;
				z = z + zSlopeX;
			}

			/*
			 * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
			 * ySlopeY and zSlopeY values:
			 */
			x0 = x0 + xSlopeY;
			y0 = y0 + ySlopeY;
			z0 = z0 + zSlopeY;
		}
	}

	private static ModelImage blur(final ModelImage image, final int sigma) {
		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		imageName = imageName + "_gblur";

		float[] sigmas = new float[] {sigma, sigma};
		if ( image.getNDims() == 3 )
		{
			sigmas = new float[] {sigma, sigma, sigma * getCorrectionFactor(image)};
		}
		OpenCLAlgorithmGaussianBlur blurAlgo;

		final ModelImage resultImage = new ModelImage(image.getType(), image.getExtents(), imageName);
		JDialogBase.updateFileInfo(image, resultImage);
		blurAlgo = new OpenCLAlgorithmGaussianBlur(resultImage, image, sigmas, true, true, false);

		blurAlgo.setRed(true);
		blurAlgo.setGreen(true);
		blurAlgo.setBlue(true);
		blurAlgo.run();

		return blurAlgo.getDestImage();
	}
	/**
	 * Returns the amount of correction which should be applied to the z-direction sigma (assuming that correction is
	 * requested).
	 * 
	 * @return the amount to multiply the z-sigma by to correct for resolution differences
	 */
	private static float getCorrectionFactor(final ModelImage image) {
		final int index = image.getExtents()[2] / 2;
		final float xRes = image.getFileInfo(index).getResolutions()[0];
		final float zRes = image.getFileInfo(index).getResolutions()[2];

		return xRes / zRes;
	}



}