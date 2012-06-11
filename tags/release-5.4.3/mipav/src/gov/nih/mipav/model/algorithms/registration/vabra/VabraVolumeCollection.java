package gov.nih.mipav.model.algorithms.registration.vabra;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;

import java.io.IOException;
import java.util.List;

public class VabraVolumeCollection implements Cloneable {

	public float[] data;
	public int XN, YN, ZN, numOfBins;
	private int chInterpType;

	protected double maxValsD;
	protected double minValsD;
	protected double intervalsD;



	public VabraVolumeCollection(ModelImage vol, int interpType, int numOfBins, boolean bMinMax) {
		this.numOfBins = numOfBins;

		minValsD = vol.getMin();
		maxValsD = vol.getMax();
		intervalsD = (maxValsD - minValsD+1) / ((double)numOfBins);

		XN = vol.getExtents().length > 0 ? vol.getExtents()[0] : 1;
		YN = vol.getExtents().length > 1 ? vol.getExtents()[1] : 1;
		ZN = vol.getExtents().length > 2 ? vol.getExtents()[2] : 1;
		this.chInterpType = interpType;
		
		data = new float[XN*YN*ZN];
		try {
			vol.exportData( 0, XN*YN*ZN, data );
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	// this constructor only allocates memory -- no read from disk
	public VabraVolumeCollection(int XN, int YN, int ZN, int interpType, int numOfBins) {
		this.XN = XN;
		this.YN = YN;
		this.ZN = ZN;
		this.numOfBins = numOfBins;
		this.chInterpType = interpType;
	}

	public void disposeLocal()
	{
		data = null;
	}


	public void downSample(int newSizeX, int newSizeY, int newSizeZ, double sigma) {

		int i, j, k;
		int ax, ay, az, x, y, z;
		int cx, cy, cz;
		double tempVal;
		int filterSize;
		double gaussKernel[][][];
		double gaussKernel1D[];

		int oldSizeX = XN, oldSizeY = YN, oldSizeZ = ZN;

		// current image data will be replace by new data
		if (newSizeX == XN
				&& newSizeY == YN
				&& newSizeZ == ZN) {
			return;
		}
		float[] newData = new float[newSizeX*newSizeY*newSizeZ];
		float[] oldData = data;

		// the following Gaussian blurring code was cut and pasted from Xiao
		// Feng Liu's
		// ABRA implementation
		ax = (int) Math.floor(oldSizeX / (float) newSizeX + 0.5);
		ay = (int) Math.floor(oldSizeY / (float) newSizeY + 0.5);
		az = (int) Math.floor(oldSizeZ / (float) newSizeZ + 0.5);

		if (ax == 1) {
			filterSize = 2;

		} else {
			filterSize = ax;
		}
		sigma = Math.pow(2, Math.log(ax) / (2 * Math.log(2.0)) - 1) * sigma;

		gaussKernel1D = new double[2 * filterSize + 1];
		gaussKernel = new double[2 * filterSize + 1][2 * filterSize + 1][2 * filterSize + 1];
		for (i = -filterSize; i <= filterSize; i++) {
			gaussKernel1D[i + filterSize] = (1.0 / (Math.sqrt(2 * Math.PI) * sigma))
					* Math.exp(-i * i / (2 * sigma * sigma));
		}

		tempVal = 0.0;
		for (k = -filterSize; k <= filterSize; k++) {
			for (j = -filterSize; j <= filterSize; j++) {
				for (i = -filterSize; i <= filterSize; i++) {
					gaussKernel[k + filterSize][j + filterSize][i + filterSize] = gaussKernel1D[k+ filterSize]
							* gaussKernel1D[j + filterSize] * gaussKernel1D[i + filterSize];
					tempVal += gaussKernel[k + filterSize][j + filterSize][i+ filterSize];
				}
			}
		}
		for (k = -filterSize; k <= filterSize; k++) {
			for (j = -filterSize; j <= filterSize; j++) {
				for (i = -filterSize; i <= filterSize; i++) {
					gaussKernel[k + filterSize][j + filterSize][i + filterSize] /= tempVal;
				}
			}
		}

		float[] tmpOld;
		float[] tmpNew;
		int index;
		for (i = 0; i < newSizeX; i++) {
			cx = i * ax;
			for (j = 0; j < newSizeY; j++) {
				cy = j * ay;
				for (k = 0; k < newSizeZ; k++) {
					cz = k * az;
					tmpOld = oldData;
					tmpNew = newData;
					tempVal = 0;
					for (z = -filterSize; z <= filterSize; z++) {
						if (z + cz < 0 || z + cz > oldSizeZ - 1)
							continue;
						for (y = -filterSize; y <= filterSize; y++) {
							if (y + cy < 0 || y + cy > oldSizeY - 1)
								continue;
							for (x = -filterSize; x <= filterSize; x++) {
								if (x + cx < 0 || x + cx > oldSizeX - 1)
									continue;
								index = (cz + z) * XN * YN + (cy + y) * XN + (cx + x);
								//tempVal += gaussKernel[filterSize + z][filterSize + y][filterSize + x]
								//		* tmpOld.getDouble(cx + x, cy + y, cz + z);
								tempVal += gaussKernel[filterSize + z][filterSize + y][filterSize + x]
										* tmpOld[index];
							}
						}
					}
					index = k * newSizeX * newSizeY + j * newSizeX + i;
					//tmpNew.set(i, j, k, (int) Math.floor(tempVal + 0.5));
					tmpNew[index] = (int) Math.floor(tempVal + 0.5);
				}
			}
		}
		data = null;
		data = newData;
		XN = newSizeX;
		YN = newSizeY;
		ZN = newSizeZ;

	}

	public void interpolate(double x, double y, double z, double[] results) {
		results[0] = RegistrationUtilities.Interpolation(data, XN, YN, ZN, x, y,z, chInterpType);
	}

	public int calculateBin(double val, int ch) {
		return (int) Math.floor((val - minValsD) / intervalsD);
	}



	// rescale the image data to fall between 0 and bins-1
	public void rescaleToBins() {
		int k, j, i, index;
		for (i = 0; i < XN; i++) for (j = 0; j < YN; j++) for (k = 0; k < ZN; k++) {
			index = k * XN * YN + j * XN + i;
			data[index] = calculateBin(data[index],0);
		}
	}

	// for convenience -- make a copy
	public VabraVolumeCollection clone() {
		VabraVolumeCollection copy = new VabraVolumeCollection(XN, YN, ZN, chInterpType, numOfBins);

		copy.data = data.clone();
		copy.maxValsD = maxValsD;
		copy.minValsD = minValsD;
		copy.intervalsD = intervalsD;

		return copy;
	}

	public VabraVolumeCollection returnDeformedCopy(float[] defField) {
		VabraVolumeCollection a = new VabraVolumeCollection(XN, YN, ZN, chInterpType, numOfBins);
		float[] newVol = new float[XN * YN * ZN];
		RegistrationUtilities.DeformImage3D(this.data, newVol, defField, XN, YN, ZN, chInterpType);
		a.data = newVol;
		a.calculateMaxAndMinVals();
		return a;
	}


	public void calculateMaxAndMinVals() {

		int ch;
		int i, j, k;

		double max = Double.NEGATIVE_INFINITY;
		double min = Double.POSITIVE_INFINITY;
		double val;
		int slice = XN * YN;
		for (i = 0; i < XN; i++) for (j = 0; j < YN; j++) for (k = 0; k < ZN; k++) {
			//val = data.getDouble(i, j, k);
			val = data[k * slice + j * XN + i];
			if ( val > max) max = val;
			if ( val < min) min = val;
		}
		minValsD = min;
		maxValsD = max;
		intervalsD = (maxValsD - minValsD+1) / ((double)numOfBins);
	}

	public int getXN() {return XN;}
	public int getYN() {return YN;}
	public int getZN() {return ZN;}
	
	public double getMax( )
	{
		return maxValsD;
	}
	
	public double getMin( )
	{
		return minValsD;
	}
	
	public double getIntervals( )
	{
		return intervalsD;
	}
	
	public void setMinMax( double maxValsDOld, double minValsDOld, double intervalsDOld )
	{
		this.maxValsD = maxValsDOld;
		this.minValsD = minValsDOld;
		this.intervalsD = intervalsDOld;
	}
}
