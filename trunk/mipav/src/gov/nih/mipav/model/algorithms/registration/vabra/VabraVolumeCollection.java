package gov.nih.mipav.model.algorithms.registration.vabra;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;

import java.util.List;

public class VabraVolumeCollection implements Cloneable {

	public ModelImage data[];
	private int XN, YN, ZN, numOfCh, numOfBins;
	private int[] chInterpType;

	protected double[] maxValsD;
	protected double[] minValsD;
	protected double[] intervalsD;



	public VabraVolumeCollection(List<ModelImage> vols, int[] interpType, int numOfBins) {
		this.numOfCh = vols.size();
		this.numOfBins = numOfBins;
		data = new ModelImage[numOfCh];
		for (int i = 0; i < vols.size(); i++) {
			data[i] = (ModelImage)(vols.get(i).clone());
		}
		XN = data[0].getExtents().length > 0 ? data[0].getExtents()[0] : 1;
		YN = data[0].getExtents().length > 1 ? data[0].getExtents()[1] : 1;
		ZN = data[0].getExtents().length > 2 ? data[0].getExtents()[2] : 1;
		this.chInterpType = interpType;

		minValsD = new double[numOfCh];
		maxValsD = new double[numOfCh];
		intervalsD = new double[numOfCh];
		calculateMaxAndMinVals();


	}

	// this constructor only allocates memory -- no read from disk
	public VabraVolumeCollection(int XN, int YN, int ZN, int channels, int[] interpType, int numOfBins) {
		this.XN = XN;
		this.YN = YN;
		this.ZN = ZN;
		this.numOfCh = channels;
		this.numOfBins = numOfBins;
		this.chInterpType = interpType;
		data = new ModelImage[channels];
		minValsD = new double[numOfCh];
		maxValsD = new double[numOfCh];
		intervalsD = new double[numOfCh];

	}

	public void disposeLocal()
	{
		for ( int i = 0; i < data.length; i++ )
		{
			if ( data[i] != null )
			{
				data[i].disposeLocal();
				data[i] = null;
			}
		}
		data = null;
		chInterpType = null;
		maxValsD = null;
		minValsD = null;
		intervalsD = null;		
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
		ModelImage[] newData = new ModelImage[numOfCh];

		int rows = data[0].getExtents().length > 0 ? data[0].getExtents()[0] : 1;
		int cols = data[0].getExtents().length > 1 ? data[0].getExtents()[1] : 1;
		int slices = data[0].getExtents().length > 2 ? data[0].getExtents()[1] : 1;
		if (data.length > 0 && newSizeX == rows
				&& newSizeY == cols
				&& newSizeZ == slices) {
			return;
		}
		for (int n = 0; n < numOfCh; n++) {
			newData[n] = new ModelImage( ModelStorageBase.FLOAT, new int[]{newSizeX, newSizeY, newSizeZ}, "downSample");
		}
		ModelImage oldData[] = data;

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

		ModelImage tmpOld;
		ModelImage tmpNew;

		for (i = 0; i < newSizeX; i++) {
			cx = i * ax;
			for (j = 0; j < newSizeY; j++) {
				cy = j * ay;
				for (k = 0; k < newSizeZ; k++) {
					cz = k * az;
					for (int ch = 0; ch < numOfCh; ch++) {
						tmpOld = oldData[ch];
						tmpNew = newData[ch];
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
									tempVal += gaussKernel[filterSize + z][filterSize + y][filterSize + x]
											* tmpOld.getDouble(cx + x, cy + y, cz + z);
								}
							}
						}
						tmpNew.set(i, j, k, (int) Math.floor(tempVal + 0.5));
					}
				}
			}
		}
		for ( i = 0; i < data.length; i++ )
		{
			data[i].disposeLocal();
		}
		data = newData;
		XN = newSizeX;
		YN = newSizeY;
		ZN = newSizeZ;

	}

	public void interpolate(double x, double y, double z, double[] results) {

		for(int ch = 0; ch < numOfCh; ch++){
			results[ch] = RegistrationUtilities.Interpolation(data[ch], XN, YN, ZN, x, y,z, chInterpType[ch]);
		}
	}

	public int calculateBin(double val, int ch) {
		return (int) Math.floor((val - minValsD[ch]) / intervalsD[ch]);
	}



	// rescale the image data to fall between 0 and bins-1
	public void rescaleToBins() {
		int k, j, i, ch;

		for (ch = 0; ch < numOfCh; ch++) {
			for (i = 0; i < XN; i++) for (j = 0; j < YN; j++) for (k = 0; k < ZN; k++) {
				data[ch].set(i, j, k, calculateBin(data[ch].getDouble(i, j, k),ch));
			}
		}
	}

	// for convenience -- make a copy
	public VabraVolumeCollection clone() {
		VabraVolumeCollection copy = new VabraVolumeCollection(XN, YN, ZN, numOfCh, chInterpType, numOfBins);

		// a.printInfo();
		for (int ch = 0; ch < numOfCh; ch++) {
			copy.data[ch] = (ModelImage)data[ch].clone();
			copy.maxValsD[ch] = maxValsD[ch];
			copy.minValsD[ch] = minValsD[ch];
			copy.intervalsD[ch] = intervalsD[ch];
		}

		return copy;
	}

	public VabraVolumeCollection returnDeformedCopy(ModelImage defField) {
		VabraVolumeCollection a = new VabraVolumeCollection(XN, YN, ZN, numOfCh, chInterpType, numOfBins);
		for (int ch = 0; ch < numOfCh; ch++) {
			ModelImage newVol = new ModelImage( ModelStorageBase.FLOAT, new int[]{XN, YN, ZN}, "returnDeformedCopy");
			RegistrationUtilities.DeformImage3D(this.data[ch], newVol, defField, XN, YN, ZN, chInterpType[ch]);
			a.data[ch] = newVol;
		}
		a.calculateMaxAndMinVals();
		return a;
	}


	public void calculateMaxAndMinVals() {

		int ch;
		int i, j, k;

		for (ch = 0; ch < numOfCh; ch++) {
			double max = Double.NEGATIVE_INFINITY;
			double min = Double.POSITIVE_INFINITY;

			for (i = 0; i < XN; i++) for (j = 0; j < YN; j++) for (k = 0; k < ZN; k++) {
				if (data[ch].getDouble(i, j, k) > max) max = data[ch].getDouble(i, j, k);
				if (data[ch].getDouble(i, j, k) < min) min = data[ch].getDouble(i, j, k);
			}
			minValsD[ch] = min;
			maxValsD[ch] = max;
			intervalsD[ch] = (maxValsD[ch] - minValsD[ch]+1) / ((double)numOfBins);
		}						
	}

	public int getXN() {return XN;}
	public int getYN() {return YN;}
	public int getZN() {return ZN;}
	public int getNumOfCh() {return numOfCh;}
}
