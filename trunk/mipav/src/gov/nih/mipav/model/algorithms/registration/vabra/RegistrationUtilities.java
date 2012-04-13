package gov.nih.mipav.model.algorithms.registration.vabra;

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

	/*
	public static int calculateBin(double interval, double minVal, double val) {
		return (int) Math.round((val - minVal) / interval);
	}
	*/

	/* imA is assumed to be from 0 and numBin-1 */
	/* imB is assumed to be from 0 and numBin-1 */
	public static void JointHistogram3D(ModelImage imA, ModelImage imB,
			int numBin, int[] roi, int[][] jointHist) {
		short val;
		int i, j, k, binA, binB;
		for (j = 0; j < numBin; j++) {
			for (i = 0; i < numBin; i++) {
				jointHist[j][i] = 0;
			}
		}
		for (k = roi[4]; k <= roi[5]; k++) {
			for (j = roi[2]; j <= roi[3]; j++) {
				for (i = roi[0]; i <= roi[1]; i++) {
					binA = imA.getUByte(i, j, k);
					//binA = (binA<0?0:binA>255?255:binA);
					
					if (binA >= numBin)
						binA = numBin - 1;
					if (binA < 0)
						binA = 0;
					
					
					binB = imB.getUByte(i, j, k);
					//binB = (binB<0?0:binB>255?255:binB);
					
					if (binB >= numBin)
						binB = numBin - 1;
					if (binB < 0)
						binB = 0;
					jointHist[binA][binB] += 1;
				}
			}
		}
	}

	public static void JointHistogram3D(ModelImage imA[], ModelImage imB[],
			int channel, int numBin, int[] roi, int[][][] jointHist) {
		JointHistogram3D(imA[channel], imB[channel], numBin, roi,
				jointHist[channel]);
	}

	/* the max value is assumed to be numBin-1, and min value 0 */
	public static void Histogram3D(ModelImage im, int numBin, int[] roi,
			int[] hist) {
		int i, j, k;
		int bin;
		for (i = 0; i < numBin; i++)
			hist[i] = 0;
		for (k = roi[4]; k <= roi[5]; k++) {
			for (j = roi[2]; j <= roi[3]; j++) {
				for (i = roi[0]; i <= roi[1]; i++) {
					bin = im.getUByte(i, j, k);
					//bin = (bin<0?0:bin>255?255:bin);
					
					if (bin >= numBin)
						bin = numBin - 1;
					if (bin < 0)
						bin = 0;
					hist[bin] += 1;					
				}
			}
		}
	}

	public static void Histogram3D(ModelImage im[], int channel, int numBin,
			int[] roi, int[][] hist) {
		Histogram3D(im[channel], numBin, roi, hist[channel]);

	}
/*
	public static ImageDataFloat DeformImageFloat3D(ImageData im,
			ImageDataFloat DF) {
		ImageDataFloat vol = new ImageDataFloat(im.getName() + "_def", im
				.getRows(), im.getRows(), im.getSlices());
		vol.setHeader(im.getHeader());
		DeformImage3D(im, vol, DF, im.getRows(), im.getCols(), im.getSlices(), RegistrationUtilities.InterpolationType.TRILINEAR);
		return vol;
	}
*/
	/*
	public static ModelImage DeformImage3D(ModelImage im, ModelImage DF, int type) {
		ModelImage vol = new ModelImage( im.getType(), im.getExtents(), im.getImageName() + "_def" );
		DeformImage3D(im, vol, DF, im.getExtents()[0], im.getExtents()[1], im.getExtents()[2], type);
		return vol;
	}
	*/

	public static void DeformImage3D(ModelImage im, ModelImage deformedIm,
			ModelImage DF, int sizeX, int sizeY, int sizeZ, int type) {
		int i, j, k;
		for (i = 0; i < sizeX; i++) 
			for (j = 0; j < sizeY; j++) 
				for (k = 0; k < sizeZ; k++) {
			if (DF.getFloat(i,j,k,0) != 0 || DF.getFloat(i,j,k,1) != 0
					|| DF.getFloat(i,j,k,2) != 0) {
				deformedIm.set(i, j, k, Interpolation(im, sizeX,
						sizeY, sizeZ, i + DF.getFloat(i,j,k,0), j
						+ DF.getFloat(i,j,k,1), k
						+ DF.getFloat(i,j,k,2), type));
			} else {
				deformedIm.set(i, j, k, im.getDouble(i, j, k));
			}
		}
	}

/*
	public static void DeformationFieldResample3DM(float[][][][] oldDF,
			ModelImage newDF) {

		int oldSizeX = oldDF.length;
		int oldSizeY = oldDF[0].length;
		int oldSizeZ = oldDF[0][0].length;


		int newSizeX = newDF.getExtents()[0];
		int newSizeY = newDF.getExtents()[1];
		int newSizeZ = newDF.getExtents()[2];

		int i, j, k, dim;
		float x, y, z;
		float ratio[] = new float[3];
		//		ratio[0] = (float) Math.floor((float) newSizeX / oldSizeX + 0.5);
		//		ratio[1] = (float) Math.floor((float) newSizeY / oldSizeY + 0.5);
		//		ratio[2] = (float) Math.floor((float) newSizeZ / oldSizeZ + 0.5);

		ratio[0] = (float) newSizeX / oldSizeX;
		ratio[1] = (float) newSizeY / oldSizeY;
		ratio[2] = (float) newSizeZ / oldSizeZ;
		

		ModelImage oldDFX = new ModelImage(ModelStorageBase.FLOAT, new int[]{oldSizeX, oldSizeY, oldSizeZ}, "oldDFX");
		ModelImage oldDFY = new ModelImage(ModelStorageBase.FLOAT, new int[]{oldSizeX, oldSizeY, oldSizeZ}, "oldDFY");
		ModelImage oldDFZ = new ModelImage(ModelStorageBase.FLOAT, new int[]{oldSizeX, oldSizeY, oldSizeZ}, "oldDFZ");

		for(i=0; i < oldSizeX; i++) 
			for(j=0; j < oldSizeY; j++) 
				for(k=0; k < oldSizeZ; k++)
				{
					oldDFX.set( i, j, k, oldDF[i][j][k][0] );
					oldDFY.set( i, j, k, oldDF[i][j][k][1] );
					oldDFZ.set( i, j, k, oldDF[i][j][k][2] );
				}

		for (i = 0; i < newSizeX; i++) {
			x = ((i+1) / ratio[0])-1;
			for (j = 0; j < newSizeY; j++) {
				y = ((j+1) / ratio[1])-1;
				for (k = 0; k < newSizeZ; k++) {
					z = ((k+1) / ratio[2])-1;
					newDF.set( i, j, k, 0,  (float) (Interpolation(oldDFX, oldSizeX, oldSizeY, oldSizeZ, x, y, z, InterpolationType.TRILINEAR) * ratio[0]) );
					newDF.set( i, j, k, 1,  (float) (Interpolation(oldDFY, oldSizeX, oldSizeY, oldSizeZ, x, y, z, InterpolationType.TRILINEAR) * ratio[1]) );
					newDF.set( i, j, k, 2,  (float) (Interpolation(oldDFZ, oldSizeX, oldSizeY, oldSizeZ, x, y, z, InterpolationType.TRILINEAR) * ratio[2]) );
				}
			}
		}
	}
	*/
	
	public static void DeformationFieldResample3DM(ModelImage oldDF,
			ModelImage newDF, int oldSizeX, int oldSizeY, int oldSizeZ,
			int newSizeX, int newSizeY, int newSizeZ) {

		int i, j, k, dim;
		float x, y, z;
		float ratio[] = new float[3];
		//		ratio[0] = (float) Math.floor((float) newSizeX / oldSizeX + 0.5);
		//		ratio[1] = (float) Math.floor((float) newSizeY / oldSizeY + 0.5);
		//		ratio[2] = (float) Math.floor((float) newSizeZ / oldSizeZ + 0.5);

		ratio[0] = (float) newSizeX / oldSizeX;
		ratio[1] = (float) newSizeY / oldSizeY;
		ratio[2] = (float) newSizeZ / oldSizeZ;


		ModelImage oldDFX = new ModelImage(ModelStorageBase.FLOAT, new int[]{oldSizeX, oldSizeY, oldSizeZ}, "oldDFX");
		ModelImage oldDFY = new ModelImage(ModelStorageBase.FLOAT, new int[]{oldSizeX, oldSizeY, oldSizeZ}, "oldDFY");
		ModelImage oldDFZ = new ModelImage(ModelStorageBase.FLOAT, new int[]{oldSizeX, oldSizeY, oldSizeZ}, "oldDFZ");

		for(i=0; i < oldSizeX; i++) 
			for(j=0; j < oldSizeY; j++) 
				for(k=0; k < oldSizeZ; k++)
				{
					oldDFX.set( i, j, k, oldDF.getFloat(i, j, k, 0) );
					oldDFY.set( i, j, k, oldDF.getFloat(i, j, k, 1) );
					oldDFZ.set( i, j, k, oldDF.getFloat(i, j, k, 2) );
				}
		
		
		for (i = 0; i < newSizeX; i++) {
			x = ((i+1) / ratio[0])-1;
			for (j = 0; j < newSizeY; j++) {
				y = ((j+1) / ratio[1])-1;
				for (k = 0; k < newSizeZ; k++) {
					z = ((k+1) / ratio[2])-1;
					newDF.set( i, j, k, 0,  (float) (Interpolation(oldDFX, oldSizeX, oldSizeY, oldSizeZ, x, y, z, InterpolationType.TRILINEAR) * ratio[0]) );
					newDF.set( i, j, k, 1,  (float) (Interpolation(oldDFY, oldSizeX, oldSizeY, oldSizeZ, x, y, z, InterpolationType.TRILINEAR) * ratio[1]) );
					newDF.set( i, j, k, 2,  (float) (Interpolation(oldDFZ, oldSizeX, oldSizeY, oldSizeZ, x, y, z, InterpolationType.TRILINEAR) * ratio[2]) );
				}
			}
		}

		oldDFX.disposeLocal(); oldDFX = null;
		oldDFY.disposeLocal(); oldDFX = null;
		oldDFZ.disposeLocal(); oldDFX = null;
	}

	//Scatter point interp using Shepard's method
	//scatterpoint list array should represent {xLocation, yLocation, zLocation, xComponentOfVector,yComponentOfVector,zComponentOfVector}
/*
	public static void scatterPointInterp(ModelImage outField, List<double[]> scatterPointsList,int p, double R){

		double[] newVec = new double[3];
		double weightSum;
		double currentWeight=0;
		double[] currentPoint;
		Iterator<double[]> itr;
		//int listN = scatterPointsList.size();
		for(int i = 0; i < outField.getExtents()[0]; i++)
			for(int j = 0; j < outField.getExtents()[1]; j++)
				for(int k = 0; k < outField.getExtents()[2]; k++){

					weightSum=0;	
					for(int c = 0; c < 3; c++) newVec[c] = 0;
					itr = scatterPointsList.iterator();
					while(itr.hasNext()){
						currentPoint = itr.next();
						
						//use p-norm
						currentWeight = Math.pow(Math.pow((i-currentPoint[0]),p) 
								+ Math.pow((j-currentPoint[1]),p) 
								+ Math.pow((k-currentPoint[2]),p),1/(double)p);

						if(currentWeight == 0){
							for(int c = 0; c < 3; c++) outField.set(i,j,k,c, currentPoint[c+3]);
							break;
						}else if(currentWeight < R){
						//}else if(true){
							//if(q<2){
							//	System.out.format(q+"\n");
							//	System.out.format(currentWeight+"\n");
							//System.out.format((i-currentPoint[0])+" "+(j-currentPoint[1])+" "+(k-currentPoint[2])+"\n");
							//System.out.format(Math.pow((i-currentPoint[0]),p)+" "+Math.pow((j-currentPoint[1]),p)+" "+Math.pow((k-currentPoint[2]),p)+"\n");

							//}

							//if the location exist in list, then set field as that
							//currentWeight = 1/currentWeight;

							currentWeight = Math.pow((R-currentWeight)/R*currentWeight , 2);
							weightSum += currentWeight;							
							for(int c = 0; c < 3; c++) newVec[c] += currentWeight*currentPoint[c+3];
						}
					
					}

					if(currentWeight != 0 && weightSum !=0){
						for(int c = 0; c < 3; c++) outField.set(i,j,k,c, newVec[c]/weightSum);
					}
	 
				}
		
	}
*/

/*
	public static float[][][] computeJacobianDet(float[][][][] def){
		int nx = def.length;
		int ny = def[0].length;
		int nz = def[0][0].length;
		float[][][] jacmap = new float[nx][ny][nz];
		float[][] jac = new float[3][3];
		float Dmx, Dmy, Dmz, Dpx, Dpy, Dpz, D0x, D0y, D0z;
		for(int x=0; x<nx; x++) for(int y=0; y<ny; y++) for(int z=0; z<nz; z++){
			// derivatives of the x comp wrt x, y, and z
			int LY = (y == 0) ? 1 : 0;
			int HY = (y == (ny - 1)) ? 1 : 0;
			int LZ = (z == 0) ? 1 : 0;
			int HZ = (z == (nz - 1)) ? 1 : 0;
			int LX = (x == 0) ? 1 : 0;
			int HX = (x == (nx - 1)) ? 1 : 0;

			// DERIVATIVES OF THE X COMPONENT 
			// central differences
			D0x = (def[x + 1 - HX][y][z][0] - def[x - 1 + LX][y][z][0]) / 2;
			D0y = (def[x][y + 1 - HY][z][0] - def[x][y - 1 + LY][z][0]) / 2;
			D0z = (def[x][y][z + 1 - HZ][0] - def[x][y][z - 1 + LZ][0]) / 2;

			//set the appropriate values in the jacobian matrix
			jac[0][0]=1+D0x;
			jac[0][1]=D0y;
			jac[0][2]=D0z;

			// DERIVATIVES OF THE Y COMPONENT 
			// central differences
			D0x = (def[x + 1 - HX][y][z][1] - def[x - 1 + LX][y][z][1]) / 2;
			D0y = (def[x][y + 1 - HY][z][1] - def[x][y - 1 + LY][z][1]) / 2;
			D0z = (def[x][y][z + 1 - HZ][1] - def[x][y][z - 1 + LZ][1]) / 2;
			//set the appropriate values in the jacobian matrix
			jac[1][0]=D0x;
			jac[1][1]=1+D0y;
			jac[1][2]=D0z;

			// DERIVATIVES OF THE Z COMPONENT 
			// central differences
			D0x = (def[x + 1 - HX][y][z][2] - def[x - 1 + LX][y][z][2]) / 2;
			D0y = (def[x][y + 1 - HY][z][2] - def[x][y - 1 + LY][z][2]) / 2;
			D0z = (def[x][y][z + 1 - HZ][2] - def[x][y][z - 1 + LZ][2]) / 2;

			//set the appropriate values in the jacobian matrix
			jac[2][0]=D0x;
			jac[2][1]=D0y;
			jac[2][2]=1+D0z;

			//set the value equal to the determinant
			jacmap[x][y][z]= NumericsPublic.determinant3D(jac);

		}
		return jacmap;
	}
	*/

	
	/*
	public static float computeMaxJacobianDetRBF(float[][][] def, double[] localCoarseGradient){


		int nx = def.length;
		int ny = def[0].length;
		int nz = def[0][0].length;
		float max=0;
		float maxdiff=0;
		float temp;
		float[][] jac = new float[3][3];
		float Dmx, Dmy, Dmz, Dpx, Dpy, Dpz, D0x, D0y, D0z;
		for(int x=0; x<nx; x++) for(int y=0; y<ny; y++) for(int z=0; z<nz; z++){
			// derivatives of the x comp wrt x, y, and z
			int LY = (y == 0) ? 1 : 0;
			int HY = (y == (ny - 1)) ? 1 : 0;
			int LZ = (z == 0) ? 1 : 0;
			int HZ = (z == (nz - 1)) ? 1 : 0;
			int LX = (x == 0) ? 1 : 0;
			int HX = (x == (nx - 1)) ? 1 : 0;

			// DERIVATIVES OF THE X COMPONENT 
			// central differences
			D0x = (def[x + 1 - HX][y][z] - def[x - 1 + LX][y][z]) / 2;
			D0y = (def[x][y + 1 - HY][z] - def[x][y - 1 + LY][z]) / 2;
			D0z = (def[x][y][z + 1 - HZ] - def[x][y][z - 1 + LZ]) / 2;


			//set the appropriate values in the jacobian matrix
			jac[0][0]=D0x*(float)localCoarseGradient[0];
			jac[0][1]=D0y*(float)localCoarseGradient[0];
			jac[0][2]=D0z*(float)localCoarseGradient[0];

			//set the appropriate values in the jacobian matrix
			jac[1][0]=D0x*(float)localCoarseGradient[1];
			jac[1][1]=D0y*(float)localCoarseGradient[1];
			jac[1][2]=D0z*(float)localCoarseGradient[1];

			//set the appropriate values in the jacobian matrix
			jac[2][0]=D0x*(float)localCoarseGradient[2];
			jac[2][1]=D0y*(float)localCoarseGradient[2];
			jac[2][2]=D0z*(float)localCoarseGradient[2];


			//set the value equal to the determinant
			temp= Math.abs(NumericsPublic.determinant3D(jac));
			if (temp > max) max = temp;

		}
		return max;

	}
*/
	/*
	public static float[] computeMaxAndMinJacobianDet(float[][][][] def){

		int nx = def.length;
		int ny = def[0].length;
		int nz = def[0][0].length;
		float[] maxAndMin = new float[4];
		maxAndMin[0] = Float.MIN_VALUE;
		maxAndMin[1] = Float.MAX_VALUE;
		maxAndMin[2] = Float.MIN_VALUE;
		maxAndMin[3] = Float.MAX_VALUE;
		float temp;
		float[][] jac = new float[3][3];
		for(int x=0; x<nx; x++) for(int y=0; y<ny; y++) for(int z=0; z<nz; z++){
			// derivatives of the x comp wrt x, y, and z
			int LY = (y == 0) ? 1 : 0;
			int HY = (y == (ny - 1)) ? 1 : 0;
			int LZ = (z == 0) ? 1 : 0;
			int HZ = (z == (nz - 1)) ? 1 : 0;
			int LX = (x == 0) ? 1 : 0;
			int HX = (x == (nx - 1)) ? 1 : 0;

			// central differences
			jac[0][0] = (def[x + 1 - HX][y][z][0] - def[x - 1 + LX][y][z][0]) / 2;
			jac[0][1] = (def[x][y + 1 - HY][z][0] - def[x][y - 1 + LY][z][0]) / 2;
			jac[0][2] = (def[x][y][z + 1 - HZ][0] - def[x][y][z - 1 + LZ][0]) / 2;

			jac[1][0] = (def[x + 1 - HX][y][z][1] - def[x - 1 + LX][y][z][1]) / 2;
			jac[1][1] = (def[x][y + 1 - HY][z][1] - def[x][y - 1 + LY][z][1]) / 2;
			jac[1][2] = (def[x][y][z + 1 - HZ][1] - def[x][y][z - 1 + LZ][1]) / 2;

			jac[2][0] = (def[x + 1 - HX][y][z][2] - def[x - 1 + LX][y][z][2]) / 2;
			jac[2][1] = (def[x][y + 1 - HY][z][2] - def[x][y - 1 + LY][z][2]) / 2;
			jac[2][2] = (def[x][y][z + 1 - HZ][2] - def[x][y][z - 1 + LZ][2]) / 2;

			//set the value equal to the determinant
			temp= NumericsPublic.determinant3D(jac);
			if (temp > maxAndMin[0]) maxAndMin[0] = temp;
			if (temp < maxAndMin[1]) maxAndMin[1] = temp;

			jac[0][0]+=1;
			jac[1][1]+=1;
			jac[2][2]+=1;

			//set the value equal to the determinant
			temp= NumericsPublic.determinant3D(jac);
			if (temp > maxAndMin[2]) maxAndMin[2] = temp;
			if (temp < maxAndMin[3]) maxAndMin[3] = temp;

		}
		return maxAndMin;
	}
*/

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


	/*
	public static double Interpolation(float[][][][] oldVM,	int XN, int YN, int ZN, 
			int dim, double x, double y, double z, int type) {

		return Interpolation(oldV, XN, YN, ZN, x, y,z, type);
	}
	 */
/*
	public static double Interpolation(ModelImage oldV, double x, double y, double z, int type) {
		return Interpolation(oldV, oldV.getExtents()[0], oldV.getExtents()[1], oldV.getExtents()[2],x,y, z, type);
	}
*/
	
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

	public static class InterpolationType{
		public static final int TRILINEAR = 0;
		public static final int NEAREST_NEIGHTBOR = 1;
	}

	// Nearest Neighbor interpolation
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

	// Nearest Neighbor interpolation Boolean
	/*
	public static boolean NNInterpolationBool(boolean[][][] oldV, int XN, int YN, int ZN,
			double x, double y, double z) {
		double d000 = 0.0, d001 = 0.0, d010 = 0.0, d011 = 0.0;
		double d100 = 0.0, d101 = 0.0, d110 = 0.0, d111 = 0.0;
		boolean value = false;
		double dist = 0.0;
		int x0 = (int) x, y0 = (int) y, z0 = (int) z;
		int x1 = x0 + 1, y1 = y0 + 1, z1 = z0 + 1;


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
	*/
	
	/*
	public static int[] NNInterpolationLoc(ModelImage oldV, int XN, int YN, int ZN,
			double x, double y, double z) {
		int[] valAndLoc = new int[4];
		double d000 = 0.0, d001 = 0.0, d010 = 0.0, d011 = 0.0;
		double d100 = 0.0, d101 = 0.0, d110 = 0.0, d111 = 0.0;
		double dist = 0.0;
		int x0 = (int) x, y0 = (int) y, z0 = (int) z;
		int x1 = x0 + 1, y1 = y0 + 1, z1 = z0 + 1;


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
*/

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

/*
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
	*/
	
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

	/*
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
	*/

	//calculate bounding box of the two images
	/*
	static public int[] calculateBoundingBox(ModelImage sub, ModelImage tar) {
		int k, j, i;
		int XN, YN, ZN, ext;
		int boundingBox[] = new int[6];
		XN = sub.getExtents()[0];
		YN = sub.getExtents()[1];
		ZN = sub.getExtents()[2];
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
	}
*/

	// rescale the image data to fall between 0 and bins-1
	/*
	static public void normalize(ModelImage sub, double minVals, double maxVals, int bins) {
		int k, j, i;
		float intervals = ((float) (maxVals - minVals)) / (bins-1);
		int XN = sub.getExtents()[0];
		int YN = sub.getExtents()[1];
		int ZN = sub.getExtents()[2];

		for (i = 0; i < XN; i++) {
			for (j = 0; j < YN; j++) {
				for (k = 0; k < ZN; k++) {
					sub.set(i, j, k, RegistrationUtilities.calculateBin(intervals,
							minVals, sub.getDouble(i, j, k)));
				}
			}
		}
	}
	*/

	//Calculate Max and Min of the two images
	/*
	static public double[] calculateMinAndMaxVals(ModelImage sub, ModelImage tar) {

		int ch;
		int CH;
		int i, j, k;
		int x = 0, y = 0, z = 0;
		int mx = 0, my = 0, mz = 0;

		int XN, YN, ZN;
		XN = sub.getExtents()[0];
		YN = sub.getExtents()[1];
		ZN = sub.getExtents()[2];
		double MinandMaxValsD[] = new double[2];
		double max = Double.NEGATIVE_INFINITY;
		double min = Double.POSITIVE_INFINITY;
		for (i = 0; i < XN; i++) {
			for (j = 0; j < YN; j++) {
				for (k = 0; k < ZN; k++) {

					if (sub.getDouble(i, j, k) > max) {
						max = sub.getDouble(i, j, k);
						mx = i;
						my = j;
						mz = k;
					}
					if (sub.getDouble(i, j, k) < min) {
						min = sub.getDouble(i, j, k);
						x = i;
						y = j;
						z = k;
					}
					if (tar.getDouble(i, j, k) > max) {
						max = tar.getDouble(i, j, k);
						mx = i;
						my = j;
						mz = k;
					}
					if (tar.getDouble(i, j, k) < min) {
						min = tar.getDouble(i, j, k);
						x = i;
						y = j;
						z = k;
					}

				}
			}
		}

		MinandMaxValsD[0] = min;
		MinandMaxValsD[1] = max;
		//System.out.format("Max: " + MinandMaxValsD[0] + " Min" + MinandMaxValsD[1] + "\n");
		return MinandMaxValsD;

	}
	*/
/*
	public static ModelImage[] split4DImageDataIntoArray(ModelImage volIn){

		int XN = volIn.getExtents()[0];
		int YN = volIn.getExtents()[1];
		int ZN = volIn.getExtents()[2];
		int CH = volIn.getExtents()[3];

		ModelImage[] volInArray = new ModelImage[CH];

		for (int c = 0; c < CH; c++){
			if(volIn.getType() == ModelStorageBase.BOOLEAN){

				volInArray[c] = new ModelImage( ModelStorageBase.FLOAT, new int[]{XN, YN, ZN}, volIn.getImageName() );
				for (int i = 0; i < XN; i++) for (int j = 0; j < YN; j++)for (int k = 0; k < ZN; k++){
					if(volIn.getBoolean(i, j, k, c))volInArray[c].set(i, j, k, 1);
					else volInArray[c].set(i, j, k, 0);
				}
			}
			else{
				volInArray[c] = new ModelImage( volIn.getType(), new int[]{XN, YN, ZN}, volIn.getImageName() );
				for (int i = 0; i < XN; i++) for (int j = 0; j < YN; j++)for (int k = 0; k < ZN; k++){
					volInArray[c].set(i, j, k, volIn.get(i, j, k, c));
				}
			}
		}

		return volInArray;
	}
	*/
/*
	public static ModelImage combineImageDataArrayTo4D(ModelImage volArray[]){

		int XN = volArray[0].getExtents()[0];
		int YN = volArray[0].getExtents()[1];
		int ZN = volArray[0].getExtents()[2];
		int CH = volArray.length;

		ModelImage volOut = new ModelImage(volArray[0].getType(), new int[]{XN, YN, ZN, CH}, volArray[0].getImageName());

		for (int c = 0; c < CH; c++){
			for (int i = 0; i < XN; i++) for (int j = 0; j < YN; j++)for (int k = 0; k < ZN; k++){
				volOut.set(i, j, k, c, volArray[c].getDouble(i, j, k, c));
			}
		}
		return volOut;
	}
*/

	//Reorder the dimensions in the array
	/*
	public static ModelImage reorderDimension(ModelImage inField, boolean changeXYZTtoTXYZ){
		ModelImage outField = new ModelImage( inField.getType(), inField.getExtents(), inField.getImageName() );
		for (int i = 0; i < inField.getExtents()[0]; i ++)
			for (int j = 0; j < inField.getExtents()[1]; j ++)
				for (int k = 0; k < inField.getExtents()[2]; k ++){
					for (int c = 0; c < inField.getExtents()[3]; c ++){
						if(changeXYZTtoTXYZ)
							outField.set(c,i,j,k, inField.getFloat(i, j, k,c));
						else
							outField.set(i,j,k,c, inField.getFloat(c,i, j, k));
					}
				}
		return outField;
	}
	*/
	
	//converts Deformation to Displacement field, and vice-verse
	/*
	public static void convertField(ModelImage field, boolean defToDisp){
		int convertFactor = 1;
		//changing from deformation to Displacement
		if (defToDisp) convertFactor = -1;

		for (int i = 0; i < field.getExtents()[0]; i ++)
			for (int j = 0; j < field.getExtents()[1]; j ++)
				for (int k = 0; k < field.getExtents()[2]; k ++){
					field.set(i,j,k,0, field.getFloat(i, j, k, 0)+ convertFactor*i);
					field.set(i,j,k,1, field.getFloat(i, j, k, 1)+ convertFactor*j);
					field.set(i,j,k,2, field.getFloat(i, j, k, 2)+ convertFactor*k);
				}
	}
	*/
	/*
	public static ModelImage findFieldMagnitude(ModelImage inField){
		int XN = inField.getExtents()[0];
		int YN = inField.getExtents()[1];
		int ZN = inField.getExtents()[2];
		int CN = inField.getExtents()[3];
		ModelImage outField = new ModelImage( ModelStorageBase.FLOAT, new int[]{XN, YN, ZN}, "temp" );
		double sumOfSq;
		for (int i = 0; i < XN; i ++)
			for (int j = 0; j < YN; j ++)
				for (int k = 0; k < ZN; k ++){
					sumOfSq = 0;
					for (int c = 0; c < CN; c ++){
						sumOfSq += inField.getDouble(i, j, k, c) * inField.getDouble(i, j, k, c);
					}
					outField.set(i,j,k, Math.sqrt(sumOfSq));
				}
		return outField;
	}*/
	
	//Inverts a deformation field
	/*
	public static void invertDisplacementField(ModelImage disp, ModelImage inv){

		int maxIter, iter, XN, YN, ZN;
		float threshold, deltax, deltay, deltaz, deltanorm, deltanorm_old, x, y, z, xnew, ynew, znew;

		ModelImage[] dispFieldArray = split4DImageDataIntoArray(disp);

		System.out.format("Start iter\n");
		maxIter = 10000;
		threshold = 0.0001f;
		double[] v= new double[3];
		double[] v_new= new double[3];
		XN = disp.getExtents()[0];
		YN = disp.getExtents()[1];
		ZN = disp.getExtents()[2];
		int[] previousConverged = new int[3];
		for(int i = 0; i < XN; i++){
			for(int j = 0; j < YN; j++){ 
				for(int k = 0; k < ZN; k++){
					//Lotta's
					//initialize variables 
					deltax = 1.0f; 
					deltay = 1.0f;  
					deltaz = 1.0f; 
					deltanorm = (float) Math.sqrt(deltax*deltax + deltay*deltay + deltaz*deltaz); 
					deltanorm_old = deltanorm;
					x = (float)i - dispFieldArray[0].getFloat(i, j, k, 0);
					y = (float)j - dispFieldArray[1].getFloat(i, j, k, 1);  
					z = (float)k - dispFieldArray[2].getFloat(i, j, k, 2);
					iter = 0; 
					//////////////////////

					//	while(deltanorm > threshold){   
					while((deltanorm > threshold || (deltanorm_old - deltanorm >= 0.0f && deltanorm > 0.01))){   
						xnew = x; 
						ynew = y; 
						znew = z; 

						if(xnew > 0.0f && ynew > 0.0f && znew > 0.0f && xnew < (float)XN-1 && ynew < (float)YN-1 && znew < (float)ZN-1){
							deltax = (float)i -(xnew + (float)TrilinearInterpolation(dispFieldArray[0], XN, YN, ZN, xnew, ynew, znew)); 
							deltay = (float)j -(ynew + (float)TrilinearInterpolation(dispFieldArray[1], XN, YN, ZN, xnew, ynew, znew)); 
							deltaz = (float)k -(znew +  (float)TrilinearInterpolation(dispFieldArray[2], XN, YN, ZN, xnew, ynew, znew)); 
						} 
						else{ 
							deltax = 0.0f; 
							deltay = 0.0f;  
							deltaz = 0.0f; 
						} 

						deltanorm_old = deltanorm;
						deltanorm = (float) Math.sqrt(deltax*deltax + deltay*deltay + deltaz*deltaz); 
						x += deltax/2.0f; 
						y += deltay/2.0f;  
						z += deltaz/2.0f;  
						iter ++; 
						if(iter == maxIter){  
							//	    printf("Algorithm did not converge, deltanorm = %5.4f\n", deltanorm);	
							deltanorm_old = deltanorm;		
							xnew = (float)i + inv.getFloat(previousConverged[0],previousConverged[1],previousConverged[2],0);
							ynew = (float)j + inv.getFloat(previousConverged[0],previousConverged[1],previousConverged[2],1);
							znew = (float)k + inv.getFloat(previousConverged[0],previousConverged[1],previousConverged[2],2);
							deltax = (float)i -(xnew + (float)TrilinearInterpolation(dispFieldArray[0], XN, YN, ZN, xnew, ynew, znew)); 
							deltay = (float)j -(ynew + (float)TrilinearInterpolation(dispFieldArray[1], XN, YN, ZN, xnew, ynew, znew)); 
							deltaz = (float)k -(znew +  (float)TrilinearInterpolation(dispFieldArray[2], XN, YN, ZN, xnew, ynew, znew));
							deltanorm = (float) Math.sqrt(deltax*deltax + deltay*deltay + deltaz*deltaz); 
							if(deltanorm < deltanorm_old){
								x = xnew;
								y = ynew;
								z = znew;
								//	      printf("new used\n");
							}
							//	    printf("deltanorm = %5.4f\n", deltanorm);			
							deltanorm = 0.0f;
						}else{
							previousConverged[0] =i;
							previousConverged[1] =j;
							previousConverged[2] =k;
						}
					} 

					inv.set(i,j,k,0,x- (float)i); 
					inv.set(i,j,k,1,y - (float)j); 
					inv.set(i,j,k,2,z - (float)k);

				}
			}
		}

	}	
	*/
/*
	public static ModelImage composeDeformations(ModelImage leftDef, ModelImage rightDef){//return leftDef(rightDef(x))
		int chN = 3;		
		int XN = leftDef.getExtents()[0];
		int YN = leftDef.getExtents()[1];
		int ZN = leftDef.getExtents()[2];
		double[] leftVec = new double[chN];
		double[] rightVec = new double[chN];

		ModelImage newDef = (ModelImage)leftDef.clone();
		ModelImage[] leftDefArray = split4DImageDataIntoArray(leftDef);

		for(int i = 0; i < XN; i++)
			for(int j = 0; j < YN; j++)
				for(int k = 0; k < ZN; k++){

					for(int c = 0; c < chN; c++) rightVec[c] = rightDef.getDouble(i, j, k, c); 

					//get current deformation at where the new deformation is pointing
					for(int c = 0; c < chN; c++) leftVec[c] = RegistrationUtilities.Interpolation(leftDefArray[c], XN, YN, ZN, 
							rightVec[0]+i, rightVec[1]+j, rightVec[2]+k, InterpolationType.TRILINEAR); 

					for(int c = 0; c < chN; c++) newDef.set(i,j,k, c, rightVec[c]+leftVec[c]);

				}
		return newDef;			

	}
*/
	//Finds all labels in an Image. 
	//Input: Labeled/Segmentation Image
	//Output: List of Labels
	/*
	public static ArrayList<Integer> findLabels(ModelImage truth){
		ArrayList<Integer> labels = new ArrayList<Integer>();
		if(truth!=null){
			int rows = truth.getExtents()[0];
			int cols = truth.getExtents()[1];
			int slcs = truth.getExtents()[2];
			for(int i=0; i<rows; i++){
				for(int j=0; j<cols; j++){
					for(int k=0; k<slcs; k++){
						if(truth.getExtents()[3] > 1){
							for(int c=0; c<truth.getExtents()[3]; c++){
								if(!labels.contains(new Integer(truth.getInt(i,j,k,c)))){
									labels.add(new Integer(truth.getInt(i,j,k,c)));
								}	
							}
						}
						else{
							if(!labels.contains(new Integer(truth.getInt(i,j,k)))){
								labels.add(new Integer(truth.getInt(i,j,k)));
							}
						}
					}
				}
			}
		}else{
			System.err.println("No data!");
		}
		labels.trimToSize();
		System.out.println("Found Labels: ");
		System.out.println(labels);
		return labels;
	}
	*/
	
	/*

	public static ImageData DeformImage3DNN(ImageData im, ImageDataFloat DF) {
		ImageData vol = im.mimic();
		vol.setName(im.getName() + "_def");
		DeformImage3DNN(im, vol, DF, im.getRows(), im.getCols(), im.getSlices());
		return vol;
	}

	public static void DeformImage3DNN(ImageData im, ImageData deformedIm,
			ImageDataFloat DF, int sizeX, int sizeY, int sizeZ) {
		int i, j, k;
		float[][][][] DFM = DF.toArray4d();
		for (i = 0; i < sizeX; i++) for (j = 0; j < sizeY; j++) for (k = 0; k < sizeZ; k++) {
					if (DFM[i][j][k][0] != 0 || DFM[i][j][k][1] != 0
							|| DFM[i][j][k][2] != 0) {
						deformedIm.set(i, j, k, Interpolation(im, sizeX, sizeY,	sizeZ, i + DFM[i][j][k][0],
								j + DFM[i][j][k][1], k + DFM[i][j][k][2], interpolationType.NearestNeighbor));
					} else {
						deformedIm.set(i, j, k, im.getDouble(i, j, k));
					}
				}
	}
	 */

	/*
	public static double Interpolation(ImageData oldV, int XN, int YN,
			int ZN, float x, float y, float z, int type) {

		return Interpolation(oldV, XN, YN, ZN, (double)x, (double)y, (double)z, type);
	}
	 */

	/*
	public static double TrilinearInterpolationFloatM(float[][][][] oldVM,
			int XO, int YO, int ZO, int dim, double x, double y, double z) {
		int i0, j0, k0, i1, j1, k1;
		float dx, dy, dz, hx, hy, hz;
		float xx, yy, zz;
		xx = (float) x;
		yy = (float) y;
		zz = (float) z;
		if (xx < 0 || xx > (XO - 1) || yy < 0 || yy > (YO - 1) || zz < 0
				|| zz > (ZO - 1)) {
			return 0;
		} else {

			j1 = (int) Math.ceil(xx);
			i1 = (int) Math.ceil(yy);
			k1 = (int) Math.ceil(zz);
			j0 = (int) Math.floor(xx);
			i0 = (int) Math.floor(yy);
			k0 = (int) Math.floor(zz);
			dx = xx - j0;
			dy = yy - i0;
			dz = zz - k0;

			// Introduce more variables to reduce computation
			hx = 1.0f - dx;
			hy = 1.0f - dy;
			hz = 1.0f - dz;
			return ((oldVM[j0][i0][k0][dim] * hx + oldVM[j1][i0][k0][dim] * dx)
	 * hy + (oldVM[j0][i1][k0][dim] * hx + oldVM[j1][i1][k0][dim]
	 * dx)
	 * dy)
	 * hz
					+ ((oldVM[j0][i0][k1][dim] * hx + oldVM[j1][i0][k1][dim]
	 * dx)
	 * hy + (oldVM[j0][i1][k1][dim] * hx + oldVM[j1][i1][k1][dim]
	 * dx)
	 * dy) * dz;
		}
	}
	 */
	/*
	public static double TrilinearInterpolation(ImageData oldV, int XN, int YN,
			int ZN, float x, float y, float z) {

		return TrilinearInterpolation(oldV, XN, YN,	ZN, (double)x, (double)y, (double)z);
	}
	 */

	/*
	// buffers to reduce memory allocation
	public static void TrilinearInterpolation(ImageData oldV[], int XO, int YO,
			int ZO, int channels, double x, double y, double z, double[] res) {
		//int i0, j0, k0, i1, j1, k1;
		//float dx, dy, dz, hx, hy, hz;
		float xx, yy, zz;

		int ch;
		xx = (float) x;
		yy = (float) y;
		zz = (float) z;

	//	if (xx < 0 || xx > (XO - 1) || yy < 0 || yy > (YO - 1) || zz < 0
		//		|| zz > (ZO - 1)) {
		//	for (ch = 0; ch < channels; ch++) {
		//		res[ch] = 0;
		//	}
	//	} else {
	//		j1 = (int) Math.ceil(xx);
		//	i1 = (int) Math.ceil(yy);
	//	k1 = (int) Math.ceil(zz);
		//	j0 = (int) Math.floor(xx);
		//	i0 = (int) Math.floor(yy);
		//	k0 = (int) Math.floor(zz);
	//		dx = xx - j0;
		//	dy = yy - i0;
		//	dz = zz - k0;

			// Introduce more variables to reduce computation
		//	hx = 1.0f - dx;
		//	hy = 1.0f - dy;
		//	hz = 1.0f - dz;
			for (ch = 0; ch < channels; ch++) {

				res[ch] = Interpolation(oldV[ch], XO, YO, ZO, xx, yy, zz, interpolationType.Trilinear);
					//(int) Math
					//	.floor(((oldV[ch].getDouble(j0, i0, k0) * hx + oldV[ch]
					//			.getDouble(j1, i0, k0)
					//			* dx)
					//			* hy + (oldV[ch].getDouble(j0, i1, k0) * hx + oldV[ch]
					//			.getDouble(j1, i1, k0)
					//			* dx)
					//			* dy)
					//			* hz
					//			+ ((oldV[ch].getDouble(j0, i0, k1) * hx + oldV[ch]
					//					.getDouble(j1, i0, k1)
					//					* dx)
					//					* hy + (oldV[ch].getDouble(j0, i1, k1)
					//					* hx + oldV[ch].getDouble(j1, i1, k1)
					//					* dx)
					//					* dy) * dz + 0.5);
		//	}
		}
	}
	 */
}
