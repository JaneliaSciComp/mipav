package gov.nih.mipav.model.algorithms.registration.vabra;

import gov.nih.mipav.model.algorithms.registration.vabra.RegistrationUtilities.InterpolationType;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;

import java.util.ArrayList;
import java.util.Collections;

public class RegistrationUtilities {

	// Buffers
	//public static int i0, j0, k0, i1, j1, k1;
	//public static float dx, dy, dz, hx, hy, hz;
	//public static int ch;
	//public static float xx, yy, zz;

	public static int calculateBin(double interval, double minVal, double val) {
		return (int) Math.round((val - minVal) / interval);
	}

	/* imA is assumed to be from 0 and numBin-1 */
	/* imB is assumed to be from 0 and numBin-1 */
	public static void JointHistogram3D(float[] imA, float[] imB,
			int numBin, int[] roi, int[][] jointHist, int XN, int YN, int ZN) {
		int i, j, k, binA, binB;
		int index;
		int slice = XN * YN;
		for (j = 0; j < numBin; j++) {
			for (i = 0; i < numBin; i++) {
				jointHist[j][i] = 0;
			}
		}
		for (k = roi[4]; k <= roi[5]; k++) {
			for (j = roi[2]; j <= roi[3]; j++) {
				for (i = roi[0]; i <= roi[1]; i++) {
					index = k * slice + j * XN + i;
					binA = ((short) ((int) imA[index] & 0xff));
					//binA = imA.getUByte(i, j, k);
					if (binA >= numBin)
						binA = numBin - 1;
					if (binA < 0)
						binA = 0;
					binB = ((short) ((int) imB[index] & 0xff));
					//binB = imB.getUByte(i, j, k);
					if (binB >= numBin)
						binB = numBin - 1;
					if (binB < 0)
						binB = 0;
					jointHist[binA][binB] += 1;
				}
			}
		}
	}

	public static void JointHistogram3D(float[] imA, float[] imB,
			int channel, int numBin, int[] roi, int[][][] jointHist, int XN, int YN, int ZN) {
		JointHistogram3D(imA, imB, numBin, roi,
				jointHist[channel], XN, YN, ZN);
	}

	/* the max value is assumed to be numBin-1, and min value 0 */
	public static void Histogram3D(float[] im, int numBin, int[] roi,
			int[] hist, int XN, int YN, int ZN) {
		int i, j, k;
		int bin;
		int index;
		int slice = XN * YN;
		for (i = 0; i < numBin; i++)
			hist[i] = 0;
		for (k = roi[4]; k <= roi[5]; k++) {
			for (j = roi[2]; j <= roi[3]; j++) {
				for (i = roi[0]; i <= roi[1]; i++) {
					index = k * slice + j * XN + i;
					bin = ((short) ((int) im[index] & 0xff));
					if (bin >= numBin)
						bin = numBin - 1;
					if (bin < 0)
						bin = 0;
					hist[bin] += 1;					
				}
			}
		}
	}

	public static void Histogram3D(float[] im, int channel, int numBin,
			int[] roi, int[][] hist, int XN, int YN, int ZN) {
		Histogram3D(im, numBin, roi, hist[channel], XN, YN, ZN);

	}
	
	public static void DeformImage3D(float[] im, float[] deformedIm,
			float[] DF, int sizeX, int sizeY, int sizeZ, int type) {
		int i, j, k, index;
		int size = sizeX * sizeY * sizeZ;
		int slice = sizeX * sizeY;
		for (i = 0; i < sizeX; i++) for (j = 0; j < sizeY; j++) for (k = 0; k < sizeZ; k++) {
			index = k * slice + j * sizeX + i;
			if (DF[index] != 0 || DF[size + index]!= 0
					|| DF[size * 2 + index] != 0) {
				deformedIm[index] =  (float) Interpolation(im, sizeX,
						sizeY, sizeZ, i + DF[index], j
						+ DF[size + index], k
						+ DF[size * 2 + index], type);
			} else {
				deformedIm[index] = im[index];
			}
		}
	}
	
	public static void DeformImage3D(ModelImage im, ModelImage deformedIm,
			float[] DF, int sizeX, int sizeY, int sizeZ, int type) {
		int i, j, k, index;
		int size = sizeX * sizeY * sizeZ;
		int slice = sizeX * sizeY;
		for (i = 0; i < sizeX; i++) for (j = 0; j < sizeY; j++) for (k = 0; k < sizeZ; k++) {
			index = k * slice + j * sizeX + i;
			if (DF[index] != 0 || DF[size + index]!= 0
					|| DF[size * 2 + index] != 0) {
				deformedIm.set( index,  (float) Interpolation(im, sizeX,
						sizeY, sizeZ, i + DF[index], j
						+ DF[size + index], k
						+ DF[size * 2 + index], type) );
			} else {
				deformedIm.set( index, im.getFloat(index) );
			}
		}
	}

	
	public static void DeformationFieldResample3DM(float[] oldDF,
			float[] newDF, int oldSizeX, int oldSizeY, int oldSizeZ,
			int newSizeX, int newSizeY, int newSizeZ) {

		int i, j, k, dim;
		double x, y, z;
		float ratio[] = new float[3];

		ratio[0] = (float) newSizeX / oldSizeX;
		ratio[1] = (float) newSizeY / oldSizeY;
		ratio[2] = (float) newSizeZ / oldSizeZ;
		
		int size = newSizeZ * newSizeY * newSizeX;
		int slice = newSizeX * newSizeY;
		int index;
		for (i = 0; i < newSizeX; i++) {
			x = ((i+1) / ratio[0])-1;
			for (j = 0; j < newSizeY; j++) {
				y = ((j+1) / ratio[1])-1;
				for (k = 0; k < newSizeZ; k++) {
					z = ((k+1) / ratio[2])-1;
					index = k * slice + j * newSizeX + i;
					newDF[index] = (float) (Interpolation(oldDF, oldSizeX, oldSizeY, oldSizeZ, x, y, z, 0, InterpolationType.TRILINEAR) * ratio[0]);
					newDF[size + index] = (float) (Interpolation(oldDF, oldSizeX, oldSizeY, oldSizeZ, x, y, z, 1, InterpolationType.TRILINEAR) * ratio[1]);
					newDF[size * 2 + index] =  (float) (Interpolation(oldDF, oldSizeX, oldSizeY, oldSizeZ, x, y, z, 2, InterpolationType.TRILINEAR) * ratio[2]);
				}
			}
		}
	}


	public static double DoubleDistance(double z0, double y0, double x0,
			double z1, double y1, double x1) {
		double tmp = 0.0;

		tmp += (x0 - x1) * (x0 - x1);
		tmp += (y0 - y1) * (y0 - y1);
		tmp += (z0 - z1) * (z0 - z1);

		return Math.sqrt(tmp);
	}



	public static float RBF(float r) {
		if (r > 1 || r < 0)
			return 0;
		return (float) (Math.pow(1 - r, 4) * (3 * Math.pow(r, 3) + 12 * r * r
				+ 16 * r + 4));
	}

	public static float RBF3D(int cx, int cy, int cz, int x, int y, int z,
			float scale) {

		return RBF((float) (Math.sqrt((cx - x) * (cx - x) + (cy - y) * (cy - y)
				+ (cz - z) * (cz - z)) / scale));
	}



	public static double Interpolation(float[] oldV, int XN, int YN,
			int ZN, double x, double y, double z, int c, int type) {
		switch(type){

		case InterpolationType.TRILINEAR: //Trilinear
			return TrilinearInterpolation(oldV, XN, YN,	ZN, x, y, z, c);
		case InterpolationType.NEAREST_NEIGHTBOR: //Nearest Neighbor
			return NNInterpolation(oldV, XN, YN, ZN, x, y, z, c);
		default:
			return 0;
		}
	}
	


	public static double Interpolation(ModelImage oldV, int XN, int YN,
			int ZN, double x, double y, double z, int type) {
		switch(type){

		case InterpolationType.TRILINEAR: //Trilinear
			return TrilinearInterpolation(oldV, XN, YN,	ZN, x, y, z);
		case InterpolationType.NEAREST_NEIGHTBOR: //Nearest Neighbor
			return NNInterpolation(oldV, XN, YN, ZN, x, y, z);
		default:
			return 0;
		}

	}


	public static double Interpolation(float[] oldV, int XN, int YN,
			int ZN, double x, double y, double z, int type) {
		switch(type){

		case InterpolationType.TRILINEAR: //Trilinear
			return TrilinearInterpolation(oldV, XN, YN,	ZN, x, y, z);
		case InterpolationType.NEAREST_NEIGHTBOR: //Nearest Neighbor
			return NNInterpolation(oldV, XN, YN, ZN, x, y, z);
		default:
			return 0;
		}
	}

	public static class InterpolationType{
		public static final int TRILINEAR = 0;
		public static final int NEAREST_NEIGHTBOR = 1;
	}

	// Nearest Neighbor interpolation
	public static double NNInterpolation(float[] oldV, int XN, int YN, int ZN,
			double x, double y, double z) {
		double d000 = 0.0, d001 = 0.0, d010 = 0.0, d011 = 0.0;
		double d100 = 0.0, d101 = 0.0, d110 = 0.0, d111 = 0.0;
		double value = 0.0, dist = 0.0;
		int x0 = (int) x, y0 = (int) y, z0 = (int) z;
		int x1 = x0 + 1, y1 = y0 + 1, z1 = z0 + 1;

		/*
		if (x == (double) (XN - 1))
			x1 = XN - 1;
		if (y == (double) (YN - 1))
			y1 = YN - 1;
		if (z == (double) (ZN - 1))
			z1 = ZN - 1;
		 */

		if (!((x0 < 0) || (x1 > (XN - 1)) || (y0 < 0) || (y1 > (YN - 1))
				|| (z0 < 0) || (z1 > (ZN - 1)))) {
			d000 = DoubleDistance(z, y, x, (double) z0, (double) y0,
					(double) x0);
			d100 = DoubleDistance(z, y, x, (double) z0, (double) y0,
					(double) x1);
			d010 = DoubleDistance(z, y, x, (double) z0, (double) y1,
					(double) x0);
			d110 = DoubleDistance(z, y, x, (double) z0, (double) y1,
					(double) x1);

			d001 = DoubleDistance(z, y, x, (double) z1, (double) y0,
					(double) x0);
			d101 = DoubleDistance(z, y, x, (double) z1, (double) y0,
					(double) x1);
			d011 = DoubleDistance(z, y, x, (double) z1, (double) y1,
					(double) x0);
			d111 = DoubleDistance(z, y, x, (double) z1, (double) y1,
					(double) x1);

			dist = d000;
			int slice = XN * YN;
			//value = oldV.getDouble(x0, y0, z0);
			value = oldV[z0 * slice + y0 * XN + x0];

			if (dist > d001) {
				dist = d001;
				//value = oldV.getDouble(x0, y0, z1);
				value = oldV[z1 * slice + y0 * XN + x0];
			}

			if (dist > d010) {
				dist = d010;
				//value = oldV.getDouble(x0, y1, z0);
				value = oldV[z0 * slice + y1 * XN + x0];
			}

			if (dist > d011) {
				dist = d011;
				//value = oldV.getDouble(x0, y1, z1);
				value = oldV[z1 * slice + y1 * XN + x0];
			}

			if (dist > d100) {
				dist = d100;
				//value = oldV.getDouble(x1, y0, z0);
				value = oldV[z0 * slice + y0 * XN + x1];
			}

			if (dist > d101) {
				dist = d101;
				//value = oldV.getDouble(x1, y0, z1);
				value = oldV[z1 * slice + y0 * XN + x1];
			}

			if (dist > d110) {
				dist = d110;
				//value = oldV.getDouble(x1, y1, z0);
				value = oldV[z0 * slice + y1 * XN + x1];
			}

			if (dist > d111) {
				dist = d111;
				//value = oldV.getDouble(x1, y1, z1);
				value = oldV[z1 * slice + y1 * XN + x1];
			}

			return value;
		} else {
			return 0;
		}
	}	

	public static double NNInterpolation(float[] oldV, int XN, int YN, int ZN,
			double x, double y, double z, int c) {
		double d000 = 0.0, d001 = 0.0, d010 = 0.0, d011 = 0.0;
		double d100 = 0.0, d101 = 0.0, d110 = 0.0, d111 = 0.0;
		double value = 0.0, dist = 0.0;
		int x0 = (int) x, y0 = (int) y, z0 = (int) z;
		int x1 = x0 + 1, y1 = y0 + 1, z1 = z0 + 1;

		/*
		if (x == (double) (XN - 1))
			x1 = XN - 1;
		if (y == (double) (YN - 1))
			y1 = YN - 1;
		if (z == (double) (ZN - 1))
			z1 = ZN - 1;
		 */

		int slice = XN * YN;
		int vol = c * slice * ZN;
		
		if (!((x0 < 0) || (x1 > (XN - 1)) || (y0 < 0) || (y1 > (YN - 1))
				|| (z0 < 0) || (z1 > (ZN - 1)))) {
			d000 = DoubleDistance(z, y, x, (double) z0, (double) y0,
					(double) x0);
			d100 = DoubleDistance(z, y, x, (double) z0, (double) y0,
					(double) x1);
			d010 = DoubleDistance(z, y, x, (double) z0, (double) y1,
					(double) x0);
			d110 = DoubleDistance(z, y, x, (double) z0, (double) y1,
					(double) x1);

			d001 = DoubleDistance(z, y, x, (double) z1, (double) y0,
					(double) x0);
			d101 = DoubleDistance(z, y, x, (double) z1, (double) y0,
					(double) x1);
			d011 = DoubleDistance(z, y, x, (double) z1, (double) y1,
					(double) x0);
			d111 = DoubleDistance(z, y, x, (double) z1, (double) y1,
					(double) x1);

			dist = d000;
			//value = oldV.getDouble(x0, y0, z0, c);
			value = oldV[vol + z0 * slice + y0 * XN + x0];

			if (dist > d001) {
				dist = d001;
				//value = oldV.getDouble(x0, y0, z1, c);
				value = oldV[vol + z1 * slice + y0 * XN + x0];
			}

			if (dist > d010) {
				dist = d010;
				//value = oldV.getDouble(x0, y1, z0, c);
				value = oldV[vol + z0 * slice + y1 * XN + x0];
			}

			if (dist > d011) {
				dist = d011;
				//value = oldV.getDouble(x0, y1, z1, c);
				value = oldV[vol + z1 * slice + y1 * XN + x0];
			}

			if (dist > d100) {
				dist = d100;
				//value = oldV.getDouble(x1, y0, z0, c);
				value = oldV[vol + z0 * slice + y0 * XN + x1];
			}

			if (dist > d101) {
				dist = d101;
				//value = oldV.getDouble(x1, y0, z1, c);
				value = oldV[vol + z1 * slice + y0 * XN + x1];
			}

			if (dist > d110) {
				dist = d110;
				//value = oldV.getDouble(x1, y1, z0, c);
				value = oldV[vol + z0 * slice + y1 * XN + x1];
			}

			if (dist > d111) {
				dist = d111;
				//value = oldV.getDouble(x1, y1, z1, c);
				value = oldV[vol + z1 * slice + y1 * XN + x1];
			}

			return value;
		} else {
			return 0;
		}
	}	
	
	
	// Nearest Neighbor interpolation Boolean
	public static boolean NNInterpolationBool(boolean[][][] oldV, int XN, int YN, int ZN,
			double x, double y, double z) {
		double d000 = 0.0, d001 = 0.0, d010 = 0.0, d011 = 0.0;
		double d100 = 0.0, d101 = 0.0, d110 = 0.0, d111 = 0.0;
		boolean value = false;
		double dist = 0.0;
		int x0 = (int) x, y0 = (int) y, z0 = (int) z;
		int x1 = x0 + 1, y1 = y0 + 1, z1 = z0 + 1;

		/*
		if (x == (double) (XN - 1))
			x1 = XN - 1;
		if (y == (double) (YN - 1))
			y1 = YN - 1;
		if (z == (double) (ZN - 1))
			z1 = ZN - 1;
		 */

		if (!((x0 < 0) || (x1 > (XN - 1)) || (y0 < 0) || (y1 > (YN - 1))
				|| (z0 < 0) || (z1 > (ZN - 1)))) {
			d000 = DoubleDistance(z, y, x, (double) z0, (double) y0,
					(double) x0);
			d100 = DoubleDistance(z, y, x, (double) z0, (double) y0,
					(double) x1);
			d010 = DoubleDistance(z, y, x, (double) z0, (double) y1,
					(double) x0);
			d110 = DoubleDistance(z, y, x, (double) z0, (double) y1,
					(double) x1);

			d001 = DoubleDistance(z, y, x, (double) z1, (double) y0,
					(double) x0);
			d101 = DoubleDistance(z, y, x, (double) z1, (double) y0,
					(double) x1);
			d011 = DoubleDistance(z, y, x, (double) z1, (double) y1,
					(double) x0);
			d111 = DoubleDistance(z, y, x, (double) z1, (double) y1,
					(double) x1);

			dist = d000;
			value = oldV[x0][y0][z0];

			if (dist > d001) {
				dist = d001;
				value = oldV[x0][y0][z1];
			}

			if (dist > d010) {
				dist = d010;
				value = oldV[x0][y1][z0];
			}

			if (dist > d011) {
				dist = d011;
				value = oldV[x0][y1][z1];
			}

			if (dist > d100) {
				dist = d100;
				value = oldV[x1][y0][z0];
			}

			if (dist > d101) {
				dist = d101;
				value = oldV[x1][y0][z1];
			}

			if (dist > d110) {
				dist = d110;
				value = oldV[x1][y1][z0];
			}

			if (dist > d111) {
				dist = d111;
				value = oldV[x1][y1][z1];
			}

			return value;
		} else {
			return false;
		}
	}	


	public static int[] NNInterpolationLoc(ModelImage oldV, int XN, int YN, int ZN,
			double x, double y, double z) {
		int[] valAndLoc = new int[4];
		double d000 = 0.0, d001 = 0.0, d010 = 0.0, d011 = 0.0;
		double d100 = 0.0, d101 = 0.0, d110 = 0.0, d111 = 0.0;
		double dist = 0.0;
		int x0 = (int) x, y0 = (int) y, z0 = (int) z;
		int x1 = x0 + 1, y1 = y0 + 1, z1 = z0 + 1;

		/*
		if (x == (double) (XN - 1))
			x1 = XN - 1;
		if (y == (double) (YN - 1))
			y1 = YN - 1;
		if (z == (double) (ZN - 1))
			z1 = ZN - 1;
		 */

		d000 = DoubleDistance(z, y, x, (double) z0, (double) y0,
				(double) x0);
		d100 = DoubleDistance(z, y, x, (double) z0, (double) y0,
				(double) x1);
		d010 = DoubleDistance(z, y, x, (double) z0, (double) y0,
				(double) x0);
		d110 = DoubleDistance(z, y, x, (double) z0, (double) y1,
				(double) x1);

		d001 = DoubleDistance(z, y, x, (double) z1, (double) y0,
				(double) x0);
		d101 = DoubleDistance(z, y, x, (double) z1, (double) y0,
				(double) x1);
		d011 = DoubleDistance(z, y, x, (double) z1, (double) y0,
				(double) x0);
		d111 = DoubleDistance(z, y, x, (double) z1, (double) y1,
				(double) x1);

		dist = d000;
		valAndLoc[1] = x0;
		valAndLoc[2] = y0;
		valAndLoc[3] = z0;

		if (dist > d001) {
			dist = d001;
			valAndLoc[1] = x0;
			valAndLoc[2] = y0;
			valAndLoc[3] = z1;
		}

		if (dist > d010) {
			dist = d010;
			valAndLoc[1] = x0;
			valAndLoc[2] = y1;
			valAndLoc[3] = z0;
		}

		if (dist > d011) {
			dist = d011;
			valAndLoc[1] = x0;
			valAndLoc[2] = y1;
			valAndLoc[3] = z1;
		}

		if (dist > d100) {
			dist = d100;
			valAndLoc[1] = x1;
			valAndLoc[2] = y0;
			valAndLoc[3] = z0;
		}

		if (dist > d101) {
			dist = d101;
			valAndLoc[1] = x1;
			valAndLoc[2] = y0;
			valAndLoc[3] = z1;
		}

		if (dist > d110) {
			dist = d110;
			valAndLoc[1] = x1;
			valAndLoc[2] = y1;
			valAndLoc[3] = z0;
		}

		if (dist > d111) {
			dist = d111;
			valAndLoc[1] = x1;
			valAndLoc[2] = y1;
			valAndLoc[3] = z1;
		}


		if (((x0 < 0) || (x1 > (XN - 1)) || (y0 < 0) || (y1 > (YN - 1))
				|| (z0 < 0) || (z1 > (ZN - 1)))) {
			if(x0 < 0) valAndLoc[1] = 0;
			if(x1 > (XN - 1)) valAndLoc[1] = XN-1;
			if(y0 < 0) valAndLoc[2] = 0;
			if(y1 > (YN - 1)) valAndLoc[2] = YN-1;
			if(z0 < 0) valAndLoc[3] = 0;
			if(z1 > (ZN - 1))valAndLoc[3] = ZN -1;
		}

		valAndLoc[0] = oldV.getInt(valAndLoc[1],valAndLoc[2],valAndLoc[3]);

		return valAndLoc;
	}	

	public static double TrilinearInterpolation(float[] oldV, int XN, int YN,
			int ZN, double x, double y, double z) {
		int i0, j0, k0, i1, j1, k1;
		double dx, dy, dz, hx, hy, hz;
		if (x < 0 || x > (XN - 1) || y < 0 || y > (YN - 1) || z < 0
				|| z > (ZN - 1)) {
			return 0;
		} else {
			j1 = (int) Math.ceil(x);
			i1 = (int) Math.ceil(y);
			k1 = (int) Math.ceil(z);
			j0 = (int) Math.floor(x);
			i0 = (int) Math.floor(y);
			k0 = (int) Math.floor(z);
			dx = x - j0;
			dy = y - i0;
			dz = z - k0;

			// Introduce more variables to reduce computation
			hx = 1.0f - dx;
			hy = 1.0f - dy;
			hz = 1.0f - dz;
			// Optimized below
			int slice = XN * YN;
			return   (
					//((oldV.getDouble(j0, i0, k0) * hx + 
							((oldV[k0 * slice + i0 * XN + j0] * hx + 
									//oldV.getDouble(j1, i0, k0) * dx) * hy 
									oldV[k0 * slice + i0 * XN + j1] * dx) * hy 
									//+ (oldV.getDouble(j0, i1, k0) * hx + 
											+ (oldV[k0 * slice + i1 * XN + j0] * hx + 
													//oldV.getDouble(j1, i1, k0) * dx) * dy) * hz 
													oldV[k0 * slice + i1 * XN + j1] * dx) * dy) * hz 
													//+ ((oldV.getDouble(j0, i0, k1) * hx + 
															+ ((oldV[k1 * slice + i0 * XN + j0] * hx + 
																	//oldV.getDouble(j1, i0, k1) * dx) * hy  
																	oldV[k1 * slice + i0 * XN + j1] * dx) * hy 
																	//+ (oldV.getDouble(j0, i1, k1) * hx + 
																			+ (oldV[k1 * slice + i1 * XN + j0] * hx + 
																					//oldV.getDouble(j1, i1, k1) * dx) * dy)* dz
																					oldV[k1 * slice + i1 * XN + j1] * dx) * dy)* dz
									);

		}
	}

	public static double TrilinearInterpolation(float[] oldV, int XN, int YN,
			int ZN, double x, double y, double z, int c) {
		int i0, j0, k0, i1, j1, k1;
		double dx, dy, dz, hx, hy, hz;
		if (x < 0 || x > (XN - 1) || y < 0 || y > (YN - 1) || z < 0
				|| z > (ZN - 1)) {
			return 0;
		} else {
			j1 = (int) Math.ceil(x);
			i1 = (int) Math.ceil(y);
			k1 = (int) Math.ceil(z);
			j0 = (int) Math.floor(x);
			i0 = (int) Math.floor(y);
			k0 = (int) Math.floor(z);
			dx = x - j0;
			dy = y - i0;
			dz = z - k0;

			// Introduce more variables to reduce computation
			hx = 1.0f - dx;
			hy = 1.0f - dy;
			hz = 1.0f - dz;

			int slice = XN * YN;
			int vol = c * slice * ZN;
			// Optimized below
			return   (
					//((oldV.getDouble(j0, i0, k0) * hx + 
							((oldV[vol + k0 * slice + i0 * XN + j0] * hx + 
									//oldV.getDouble(j1, i0, k0) * dx) * hy 
									oldV[vol + k0 * slice + i0 * XN + j1] * dx) * hy 
									//+ (oldV.getDouble(j0, i1, k0) * hx + 
											+ (oldV[vol + k0 * slice + i1 * XN + j0] * hx + 
													//oldV.getDouble(j1, i1, k0) * dx) * dy) * hz 
													oldV[vol + k0 * slice + i1 * XN + j1] * dx) * dy) * hz 
													//+ ((oldV.getDouble(j0, i0, k1) * hx + 
															+ ((oldV[vol + k1 * slice + i0 * XN + j0] * hx + 
																	//oldV.getDouble(j1, i0, k1) * dx) * hy  
																	oldV[vol + k1 * slice + i0 * XN + j1] * dx) * hy 
																	//+ (oldV.getDouble(j0, i1, k1) * hx + 
																			+ (oldV[vol + k1 * slice + i1 * XN + j0] * hx + 
																					//oldV.getDouble(j1, i1, k1) * dx) * dy)* dz
																					oldV[vol + k1 * slice + i1 * XN + j1] * dx) * dy)* dz
									);

		}
	}
	

	public static double TrilinearInterpolation(ModelImage oldV, int XN, int YN,
			int ZN, double x, double y, double z) {
		int i0, j0, k0, i1, j1, k1;
		double dx, dy, dz, hx, hy, hz;
		if (x < 0 || x > (XN - 1) || y < 0 || y > (YN - 1) || z < 0
				|| z > (ZN - 1)) {
			return 0;
		} else {
			j1 = (int) Math.ceil(x);
			i1 = (int) Math.ceil(y);
			k1 = (int) Math.ceil(z);
			j0 = (int) Math.floor(x);
			i0 = (int) Math.floor(y);
			k0 = (int) Math.floor(z);
			dx = x - j0;
			dy = y - i0;
			dz = z - k0;

			// Introduce more variables to reduce computation
			hx = 1.0f - dx;
			hy = 1.0f - dy;
			hz = 1.0f - dz;
			// Optimized below
			return   (((oldV.getDouble(j0, i0, k0) * hx + oldV.getDouble(j1, i0, k0) * dx) * hy 
					+ (oldV.getDouble(j0, i1, k0) * hx + oldV.getDouble(j1, i1, k0) * dx) * dy) * hz 
					+ ((oldV.getDouble(j0, i0, k1) * hx + oldV.getDouble(j1, i0, k1) * dx) * hy 
							+ (oldV.getDouble(j0, i1, k1) * hx + oldV.getDouble(j1, i1, k1) * dx) * dy)* dz);

		}
	}


	public static double NNInterpolation(ModelImage oldV, int XN, int YN, int ZN,
			double x, double y, double z) {
		double d000 = 0.0, d001 = 0.0, d010 = 0.0, d011 = 0.0;
		double d100 = 0.0, d101 = 0.0, d110 = 0.0, d111 = 0.0;
		double value = 0.0, dist = 0.0;
		int x0 = (int) x, y0 = (int) y, z0 = (int) z;
		int x1 = x0 + 1, y1 = y0 + 1, z1 = z0 + 1;

		/*
		if (x == (double) (XN - 1))
			x1 = XN - 1;
		if (y == (double) (YN - 1))
			y1 = YN - 1;
		if (z == (double) (ZN - 1))
			z1 = ZN - 1;
		 */

		if (!((x0 < 0) || (x1 > (XN - 1)) || (y0 < 0) || (y1 > (YN - 1))
				|| (z0 < 0) || (z1 > (ZN - 1)))) {
			d000 = DoubleDistance(z, y, x, (double) z0, (double) y0,
					(double) x0);
			d100 = DoubleDistance(z, y, x, (double) z0, (double) y0,
					(double) x1);
			d010 = DoubleDistance(z, y, x, (double) z0, (double) y1,
					(double) x0);
			d110 = DoubleDistance(z, y, x, (double) z0, (double) y1,
					(double) x1);

			d001 = DoubleDistance(z, y, x, (double) z1, (double) y0,
					(double) x0);
			d101 = DoubleDistance(z, y, x, (double) z1, (double) y0,
					(double) x1);
			d011 = DoubleDistance(z, y, x, (double) z1, (double) y1,
					(double) x0);
			d111 = DoubleDistance(z, y, x, (double) z1, (double) y1,
					(double) x1);

			dist = d000;
			value = oldV.getDouble(x0, y0, z0);

			if (dist > d001) {
				dist = d001;
				value = oldV.getDouble(x0, y0, z1);
			}

			if (dist > d010) {
				dist = d010;
				value = oldV.getDouble(x0, y1, z0);
			}

			if (dist > d011) {
				dist = d011;
				value = oldV.getDouble(x0, y1, z1);
			}

			if (dist > d100) {
				dist = d100;
				value = oldV.getDouble(x1, y0, z0);
			}

			if (dist > d101) {
				dist = d101;
				value = oldV.getDouble(x1, y0, z1);
			}

			if (dist > d110) {
				dist = d110;
				value = oldV.getDouble(x1, y1, z0);
			}

			if (dist > d111) {
				dist = d111;
				value = oldV.getDouble(x1, y1, z1);
			}

			return value;
		} else {
			return 0;
		}
	}	



	public static double TrilinearInterpolateDefField(ModelImage defField, int XN, int YN,
			int ZN, double x, double y, double z, int c) {
		int i0, j0, k0, i1, j1, k1;
		double dx, dy, dz, hx, hy, hz;
		if (x < 0 || x > (XN - 1) || y < 0 || y > (YN - 1) || z < 0
				|| z > (ZN - 1)) {
			return 0;
		} else {
			j1 = (int) Math.ceil(x);
			i1 = (int) Math.ceil(y);
			k1 = (int) Math.ceil(z);
			j0 = (int) Math.floor(x);
			i0 = (int) Math.floor(y);
			k0 = (int) Math.floor(z);
			dx = x - j0;
			dy = y - i0;
			dz = z - k0;

			// Introduce more variables to reduce computation
			hx = 1.0f - dx;
			hy = 1.0f - dy;
			hz = 1.0f - dz;
			// Optimized below
			return   (((defField.getDouble(j0, i0, k0,c) * hx + defField.getDouble(j1, i0, k0,c) * dx) * hy 
					+ (defField.getDouble(j0, i1, k0,c) * hx + defField.getDouble(j1, i1, k0,c) * dx) * dy) * hz 
					+ ((defField.getDouble(j0, i0, k1,c) * hx + defField.getDouble(j1, i0, k1,c) * dx) * hy 
							+ (defField.getDouble(j0, i1, k1,c) * hx + defField.getDouble(j1, i1, k1,c) * dx) * dy)* dz);

		}
	}

	public static class IndexedFloat implements Comparable<IndexedFloat> {
		public int index;
		public float val;

		public IndexedFloat(float val, int index) {
			this.index = index;
			this.val = val;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.lang.Comparable#compareTo(java.lang.Object)
		 */
		public int compareTo(IndexedFloat o) {
			return (int) Math.signum(val - o.val);
		}
	}

	public static int[] QKSort2(float[] arr) {
		ArrayList<IndexedFloat> list = new ArrayList<IndexedFloat>(arr.length);
		int index = 0;
		for (int i = 0; i < arr.length; i++) {
			list.add(new RegistrationUtilities.IndexedFloat(arr[i], i));
		}
		Collections.sort(list);
		int[] brr = new int[arr.length];
		for (IndexedFloat in : list) {
			arr[index] = in.val;
			brr[index++] = in.index;
		}
		return brr;
	}

	public static double VectorNormalization(double[] v, int n) {
		int i;
		double ret = 0;
		double thre = 0.0000001;
		for (i = 0; i < n; i++) {
			ret += v[i] * v[i];
		}
		ret = Math.sqrt(ret);
		if (ret < thre) {
			for (i = 0; i < n; i++) {
				v[i] = 0;
				ret = 0;
			}
		} else {
			for (i = 0; i < n; i++) {
				v[i] /= ret;
			}
		}
		return ret;
	}

	public static double NMI(int[][] histA, int[][] histB, int[][][] histAB,
			int channel, int numBin) {

		return NMI(histA[channel], histB[channel], histAB[channel], numBin);
	}

	public static double NMI(int[] histA, int[] histB, int[][] histAB,
			int numBin) {
		int i = 0, j = 0;
		double HA = 0, HB = 0, HAB = 0;
		int numVoxelA = 0, numVoxelB = 0, numVoxelAB = 0;
		double tmp = 0;
		for (i = 0; i < numBin; i++) {
			//System.out.format(histA[i] + "\n");
			numVoxelA += histA[i];
			numVoxelB += histB[i];
			for (j = 0; j < numBin; j++) {
				numVoxelAB += histAB[j][i];
			}
		}
		for (i = 0; i < numBin; i++) {
			if (histA[i] > 0) {
				tmp = ((double) histA[i]) / numVoxelA;
				HA -= tmp * Math.log(tmp);
			}
			if (histB[i] > 0) {
				tmp = ((double) histB[i]) / numVoxelB;
				HB -= tmp * Math.log(tmp);
			}
			for (j = 0; j < numBin; j++) {
				if (histAB[j][i] > 0) {
					tmp = ((double) histAB[j][i]) / numVoxelAB;
					HAB -= tmp * Math.log(tmp);
				}
			}
		}

		//System.out.format(HA+" "+HB+" "+HAB+" "+numVoxelAB+"\n");
		if (HA == 0 && HB == 0 && HAB == 0)
			return 2;
		else
			return (HA + HB) / HAB;
	}

	public static double MI(int[] histA, int[] histB, int[][] histAB,
			int numBin) {
		int i = 0, j = 0;
		double HA = 0, HB = 0, HAB = 0;
		int numVoxelA = 0, numVoxelB = 0, numVoxelAB = 0;
		double tmp = 0;
		for (i = 0; i < numBin; i++) {
			//System.out.format(histA[i] + "\n");
			numVoxelA += histA[i];
			numVoxelB += histB[i];
			for (j = 0; j < numBin; j++) {
				numVoxelAB += histAB[j][i];
			}
		}
		for (i = 0; i < numBin; i++) {
			if (histA[i] > 0) {
				tmp = ((double) histA[i]) / numVoxelA;
				HA -= tmp * Math.log(tmp);
			}
			if (histB[i] > 0) {
				tmp = ((double) histB[i]) / numVoxelB;
				HB -= tmp * Math.log(tmp);
			}
			for (j = 0; j < numBin; j++) {
				if (histAB[j][i] > 0) {
					tmp = ((double) histAB[j][i]) / numVoxelAB;
					HAB -= tmp * Math.log(tmp);
				}
			}
		}

		//System.out.format(HA+" "+HB+" "+HAB+" "+numVoxelAB+"\n");
		if (HA == 0 && HB == 0 && HAB == 0)
			return 2;
		else
			return (HA + HB) - HAB;
	}

	//calculate bounding box of the two images
	static public int[] calculateBoundingBox(ModelImage sub, ModelImage tar) {
		int k, j, i;
		int XN, YN, ZN, ext;
		int boundingBox[] = new int[6];
		XN = sub.getExtents().length > 0 ? sub.getExtents()[0] : 1;
		YN = sub.getExtents().length > 1 ? sub.getExtents()[1] : 1;
		ZN = sub.getExtents().length > 2 ? sub.getExtents()[2] : 1;
		ext = 5;

		boundingBox[1] = 0;
		boundingBox[0] = XN;
		boundingBox[3] = 0;
		boundingBox[2] = YN;
		boundingBox[5] = 0;
		boundingBox[4] = ZN;

		int count=0;
		for (i = 0; i < XN; i++){
			for (j = 0; j < YN; j++){
				for (k = 0; k < ZN; k++){
					// if (subject.data[k][j][i][ch] > 0 ||
					// target.data[k][j][i][ch] > 0)
					if ((Math.abs(sub.getDouble(i, j, k)) > 0.0000001) ||
							(Math.abs(tar.getDouble(i, j, k)) > 0.0000001))
						// changed to match Xiao Feng's abra implementation
					{
						count++;
						if (i < boundingBox[0])
							boundingBox[0] = i;
						if (i > boundingBox[1])
							boundingBox[1] = i;
						if (j < boundingBox[2])
							boundingBox[2] = j;
						if (j > boundingBox[3])
							boundingBox[3] = j;
						if (k < boundingBox[4])
							boundingBox[4] = k;
						if (k > boundingBox[5])
							boundingBox[5] = k;
					}
				}
			}
		}



		boundingBox[0]=Math.max(0, boundingBox[0]-ext); 			
		boundingBox[1]=Math.min(XN-1, boundingBox[1]+ext);
		boundingBox[2]=Math.max(0, boundingBox[2]-ext); 			
		boundingBox[3]=Math.min(YN-1, boundingBox[3]+ext);
		boundingBox[4]=Math.max(0, boundingBox[4]-ext); 			
		boundingBox[5]=Math.min(ZN-1, boundingBox[5]+ext);

		return boundingBox;

		/*for (i = 0; i < 6; i++) {
			System.out.format("bb[%d]:%d\n",i,boundingBox[i]);
		}*/

	}

}
