package gov.nih.mipav.model.algorithms.utilities;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;


/**
 * @author pandyan
 * algorithm for 4D Image calculation
 * 
 * 
 * The way this algorithm works is by taking 1 slice at a time from each time point and building an xyt volume.
 * It then does the math on this volume...and in this way builds the result image
 *
 */
public class Algorithm4DImageCalculator extends AlgorithmBase {
	
	 /** add **/
	 public static final int ADD = 0;
	
	 /** average **/
	 public static final int AVERAGE = 1;
	 
	 /** minimum **/
	 public static final int MINIMUM = 2;
	 
	 /** maximum **/
	 public static final int MAXIMUM = 3;
	 
	 /** std dev **/
	 public static final int STDDEV = 4;
	 
	 /** norm **/
	 public static final int NORM = 5;
	 
	 /** images **/
	 private ModelImage image, destImage;
	 
	 /** operation type **/
	 private int operationType;

	 /** whether to clip or promote **/
	 private boolean doClip;
	 
	 /** data type **/
	 private int dataType;
	 
	 private int colorFactor;
	
	
	 /**
	  * constructor
	  */
	public Algorithm4DImageCalculator() {
		
	}
	
	
	/**
	 * constructor
	 * @param image
	 * @param resultImage
	 * @param operationType
	 * @param doClip
	 */
	public Algorithm4DImageCalculator(ModelImage image, ModelImage resultImage, int operationType, boolean doClip) {
		this.image = image;
		this.destImage = resultImage;
		this.operationType = operationType;
		this.doClip = doClip;
		if(operationType == MAXIMUM || operationType == MINIMUM || operationType == AVERAGE || operationType == STDDEV) {
			doClip = true;
		}
	}
 	
	
	
	
	
	
	
	/**
	 * Run algorithm
	 */
	public void runAlgorithm() {
		
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];
		int tDim = image.getExtents()[3];
		byte[] byteBuff = null;
		short[] shortBuff = null;
		int[] intBuff = null;
		float[] floatBuff = null;
		long[] longBuff = null;
		double[] doubleBuff = null;
		byte[] byteBuffXYT = null;
		short[] shortBuffXYT = null;
		int[] intBuffXYT = null;
		float[] floatBuffXYT = null;
		long[] longBuffXYT = null;
		double[] doubleBuffXYT = null;
		dataType = image.getType();
		colorFactor = 1;
		if (image.isColorImage()) {
			colorFactor = 4;
		} else if (image.isComplexImage()) {
			colorFactor = 2;
		}
		int sliceLength = colorFactor * xDim * yDim;
		int volLength = sliceLength * zDim;
		
		if(dataType == ModelStorageBase.BYTE) {
			byteBuff = new byte[xDim * yDim];
			byteBuffXYT = new byte[xDim * yDim * tDim];
		}else if(dataType == ModelStorageBase.SHORT || dataType == ModelStorageBase.UBYTE) {
			shortBuff = new short[xDim * yDim];
			shortBuffXYT = new short[xDim * yDim * tDim];
		}else if(dataType == ModelStorageBase.INTEGER || dataType == ModelStorageBase.USHORT) {
			intBuff = new int[xDim * yDim];
			intBuffXYT = new int[xDim * yDim * tDim];
		}else if(dataType == ModelStorageBase.FLOAT) {
			floatBuff = new float[xDim * yDim];
			floatBuffXYT = new float[xDim * yDim * tDim];
		}else if (dataType == ModelStorageBase.LONG || dataType == ModelStorageBase.UINTEGER) {
			longBuff = new long[xDim * yDim];
			longBuffXYT = new long[xDim * yDim * tDim];
		}else if(dataType == ModelStorageBase.DOUBLE) {
			doubleBuff = new double[xDim * yDim];
			doubleBuffXYT = new double[xDim * yDim * tDim];
		}else if (dataType == ModelStorageBase.ARGB) {
			shortBuff = new short[4 * xDim * yDim];
			shortBuffXYT = new short[4 * xDim * yDim * tDim];	
		}else if (dataType == ModelStorageBase.ARGB_USHORT) {
			intBuff = new int[4 * xDim * yDim];
			intBuffXYT = new int[4 * xDim * yDim * tDim];	
		}
		
		
		
		try {
	
		
			for(int i=0;i<zDim;i++) {
				for(int t=0;t<tDim;t++) {
					int start = (volLength * t) + (sliceLength * i);
					if(dataType == ModelStorageBase.BYTE ) {
						image.exportData(start, sliceLength, byteBuff);
						for(int k=0;k<byteBuff.length;k++) {
							byteBuffXYT[k + (sliceLength * t)] = byteBuff[k];
						}
					}else if(dataType == ModelStorageBase.SHORT || dataType == ModelStorageBase.UBYTE ||
							dataType == ModelStorageBase.ARGB) {
						image.exportData(start, sliceLength, shortBuff);
						for(int k=0;k<shortBuff.length;k++) {
							shortBuffXYT[k + (sliceLength * t)] = shortBuff[k];
						}
					}else if(dataType == ModelStorageBase.INTEGER || dataType == ModelStorageBase.USHORT ||
							dataType == ModelStorageBase.ARGB_USHORT) {
						image.exportData(start, sliceLength, intBuff);
						for(int k=0;k<intBuff.length;k++) {
							intBuffXYT[k + (sliceLength * t)] = intBuff[k];
						}
					}else if(dataType == ModelStorageBase.FLOAT) {
						image.exportData(start, sliceLength, floatBuff);
						for(int k=0;k<floatBuff.length;k++) {
							floatBuffXYT[k + (sliceLength * t)] = floatBuff[k];
						}
					}else if(dataType == ModelStorageBase.LONG || dataType == ModelStorageBase.UINTEGER) {
						image.exportData(start, sliceLength, longBuff);
						for(int k=0;k<longBuff.length;k++) {
							longBuffXYT[k + (sliceLength * t)] = longBuff[k];
						}
					}else if(dataType == ModelStorageBase.DOUBLE) {
						image.exportData(start, sliceLength, doubleBuff);
						for(int k=0;k<doubleBuff.length;k++) {
							doubleBuffXYT[k + (sliceLength * t)] = doubleBuff[k];
						}
					}
				}
				
				
				//now do the math on the xyt volume
				if(dataType == ModelStorageBase.BYTE ) {
					if(doClip) {
						byte[] byteSliceBuff = null;
						if(operationType == ADD) {
							byteSliceBuff = doByteAddClip(byteBuffXYT);
						}else if(operationType == AVERAGE) {
							byteSliceBuff = doByteAverage(byteBuffXYT);
						}else if(operationType == MAXIMUM) {
							byteSliceBuff = doByteMax(byteBuffXYT);
						}else if(operationType == MINIMUM) {
							byteSliceBuff = doByteMin(byteBuffXYT);
						}else if(operationType == STDDEV) {
							byteSliceBuff = doByteStdDev(byteBuffXYT);
						}else if(operationType == NORM) {
							byteSliceBuff = doByteNormClip(byteBuffXYT);
						}
						destImage.importData(i*sliceLength, byteSliceBuff, false);
					}else {
						short[] shortSliceBuff = null;
						if(operationType == ADD) {
							shortSliceBuff = doByteAddPromote(byteBuffXYT);
						}else if(operationType == NORM) {
							shortSliceBuff = doByteNormPromote(byteBuffXYT);
						}
						destImage.importData(i*sliceLength, shortSliceBuff, false);
					}
				}else if(dataType == ModelStorageBase.SHORT  || dataType == ModelStorageBase.UBYTE) {
					if(doClip) {
						short[] shortSliceBuff = null;
						if(operationType == ADD) {
							shortSliceBuff = doShortAddClip(shortBuffXYT);
						}else if(operationType == AVERAGE) {
							shortSliceBuff = doShortAverage(shortBuffXYT);
						}else if(operationType == MAXIMUM) {
							shortSliceBuff = doShortMax(shortBuffXYT);
						}else if(operationType == MINIMUM) {
							shortSliceBuff = doShortMin(shortBuffXYT);
						}else if(operationType == STDDEV) {
							shortSliceBuff = doShortStdDev(shortBuffXYT);
						}else if(operationType == NORM) {
							shortSliceBuff = doShortNormClip(shortBuffXYT);
						}
						destImage.importData(i*sliceLength, shortSliceBuff, false);
					}else {
						int[] intSliceBuff = null;
						if(operationType == ADD) {
							intSliceBuff = doShortAddPromote(shortBuffXYT);
						}else if(operationType == NORM) {
							intSliceBuff = doShortNormPromote(shortBuffXYT);
						}
						destImage.importData(i*sliceLength, intSliceBuff, false);
					}
				}else if(dataType == ModelStorageBase.INTEGER || dataType == ModelStorageBase.USHORT) {
					if(doClip) {
						int[] intSliceBuff = null;
						if(operationType == ADD) {
							intSliceBuff = doIntAddClip(intBuffXYT);
						}else if(operationType == AVERAGE) {
							intSliceBuff = doIntAverage(intBuffXYT);
						}else if(operationType == MAXIMUM) {
							intSliceBuff = doIntMax(intBuffXYT);
						}else if(operationType == MINIMUM) {
							intSliceBuff = doIntMin(intBuffXYT);
						}else if(operationType == STDDEV) {
							intSliceBuff = doIntStdDev(intBuffXYT);
						}else if(operationType == NORM) {
							intSliceBuff = doIntNormClip(intBuffXYT);
						}
						destImage.importData(i*sliceLength, intSliceBuff, false);
					}else {
						long[] longSliceBuff = null;
						if(operationType == ADD) {
							longSliceBuff = doIntAddPromote(intBuffXYT);
						}else if(operationType == NORM) {
							longSliceBuff = doIntNormPromote(intBuffXYT);
						}
						destImage.importData(i*sliceLength, longSliceBuff, false);
					}
				}else if(dataType == ModelStorageBase.FLOAT) {
					if(doClip) {
						float[] floatSliceBuff = null;
						if(operationType == ADD) {
							floatSliceBuff = doFloatAddClip(floatBuffXYT);
						}else if(operationType == AVERAGE) {
							floatSliceBuff = doFloatAverage(floatBuffXYT);
						}else if(operationType == MAXIMUM) {
							floatSliceBuff = doFloatMax(floatBuffXYT);
						}else if(operationType == MINIMUM) {
							floatSliceBuff = doFloatMin(floatBuffXYT);
						}else if(operationType == STDDEV) {
							floatSliceBuff = doFloatStdDev(floatBuffXYT);
						}else if(operationType == NORM) {
							floatSliceBuff = doFloatNormClip(floatBuffXYT);
						}
						destImage.importData(i*sliceLength, floatSliceBuff, false);
					}else {
						double[] doubleSliceBuff = null;
						if(operationType == ADD) {
							doubleSliceBuff = doFloatAddPromote(floatBuffXYT);
						}else if(operationType == NORM) {
							doubleSliceBuff = doFloatNormPromote(floatBuffXYT);
						}
						destImage.importData(i*sliceLength, doubleSliceBuff, false);
					}
				} else if (dataType == ModelStorageBase.LONG || dataType == ModelStorageBase.UINTEGER) {
					long[] longSliceBuff = null;
					if(operationType == ADD) {
						longSliceBuff = doLongAddClip(longBuffXYT);
					}else if(operationType == AVERAGE) {
						longSliceBuff = doLongAverage(longBuffXYT);
					}else if(operationType == MAXIMUM) {
						longSliceBuff = doLongMax(longBuffXYT);
					}else if(operationType == MINIMUM) {
						longSliceBuff = doLongMin(longBuffXYT);
					}else if(operationType == STDDEV) {
						longSliceBuff = doLongStdDev(longBuffXYT);
					}else if(operationType == NORM) {
						longSliceBuff = doLongNormClip(longBuffXYT);
					}
					destImage.importData(i*sliceLength, longSliceBuff, false);	
				} else if (dataType == ModelStorageBase.DOUBLE) {
					double[] doubleSliceBuff = null;
					if(operationType == ADD) {
						doubleSliceBuff = doDoubleAddClip(doubleBuffXYT);
					}else if(operationType == AVERAGE) {
						doubleSliceBuff = doDoubleAverage(doubleBuffXYT);
					}else if(operationType == MAXIMUM) {
						doubleSliceBuff = doDoubleMax(doubleBuffXYT);
					}else if(operationType == MINIMUM) {
						doubleSliceBuff = doDoubleMin(doubleBuffXYT);
					}else if(operationType == STDDEV) {
						doubleSliceBuff = doDoubleStdDev(doubleBuffXYT);
					}else if(operationType == NORM) {
						doubleSliceBuff = doDoubleNormClip(doubleBuffXYT);
					}
					destImage.importData(i*sliceLength, doubleSliceBuff, false);	
				}
				else if (dataType == ModelStorageBase.ARGB) {
					if(doClip) {
						short[] shortSliceBuff = null;
						if(operationType == ADD) {
							shortSliceBuff = doShortAddClipARGB(shortBuffXYT);
						}else if(operationType == AVERAGE) {
							shortSliceBuff = doShortAverageARGB(shortBuffXYT);
						}else if(operationType == MAXIMUM) {
							shortSliceBuff = doShortMaxARGB(shortBuffXYT);
						}else if(operationType == MINIMUM) {
							shortSliceBuff = doShortMinARGB(shortBuffXYT);
						}else if(operationType == STDDEV) {
							shortSliceBuff = doShortStdDevARGB(shortBuffXYT);
						}else if(operationType == NORM) {
							shortSliceBuff = doShortNormClipARGB(shortBuffXYT);
						}
						destImage.importData(i*sliceLength, shortSliceBuff, false);
					}else {
						int[] intSliceBuff = null;
						if(operationType == ADD) {
							intSliceBuff = doShortAddPromoteARGB(shortBuffXYT);
						}else if(operationType == NORM) {
							intSliceBuff = doShortNormPromoteARGB(shortBuffXYT);
						}
						destImage.importData(i*sliceLength, intSliceBuff, false);
					}
				}
				else if (dataType == ModelStorageBase.ARGB_USHORT) {
					int[] intSliceBuff = null;
					if(operationType == ADD) {
						intSliceBuff = doIntAddClipARGB(intBuffXYT);
					}else if(operationType == AVERAGE) {
						intSliceBuff = doIntAverageARGB(intBuffXYT);
					}else if(operationType == MAXIMUM) {
						intSliceBuff = doIntMaxARGB(intBuffXYT);
					}else if(operationType == MINIMUM) {
						intSliceBuff = doIntMinARGB(intBuffXYT);
					}else if(operationType == STDDEV) {
						intSliceBuff = doIntStdDevARGB(intBuffXYT);
					}else if(operationType == NORM) {
						intSliceBuff = doIntNormClipARGB(intBuffXYT);
					}
					destImage.importData(i*sliceLength, intSliceBuff, false);	
				}
			}
			
			
			//finalize the result image
			destImage.calcMinMax();
			float[] resols = {image.getResolutions(0)[0], image.getResolutions(0)[1], image.getResolutions(0)[2]};
			
			FileInfoBase[] fileInfo = destImage.getFileInfo();

            for (int i = 0; i < destImage.getExtents()[2]; i++) {
                fileInfo[i].setModality(image.getFileInfo()[0].getModality());
                fileInfo[i].setFileDirectory(image.getFileInfo()[0].getFileDirectory());
                fileInfo[i].setEndianess(image.getFileInfo()[0].getEndianess());
                fileInfo[i].setUnitsOfMeasure(image.getFileInfo()[0].getUnitsOfMeasure());
                fileInfo[i].setResolutions(resols);
                fileInfo[i].setExtents(destImage.getExtents());
                fileInfo[i].setMax(destImage.getMax());
                fileInfo[i].setMin(destImage.getMin());
                fileInfo[i].setImageOrientation(image.getImageOrientation());
                fileInfo[i].setPixelPadValue(image.getFileInfo()[0].getPixelPadValue());
                fileInfo[i].setPhotometric(image.getFileInfo()[0].getPhotometric());
                fileInfo[i].setAxisOrientation(image.getAxisOrientation());
            }
			
			
		}catch(Exception e) {
			
		}
		
		
		setCompleted(true);
		
	}
	
	
	/**
	 * Calculates the max between 2 4d-byte-images by doing math on an xyt buffer that is built from both images
	 * @param xytBuff - the input
	 * @return - the xy result slice
	 */
	private byte[] doByteMax(byte[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		byte[] sliceBuff = new byte[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			byte max = 0;
			for(int t=0;t<tDim;t++) {
				byte pix = xytBuff[i + (sliceLength * t)];
				if(t == 0) {
					max = pix;
				}
				if(pix > max) {
					max = pix;
				}
				
				
				
			}

			sliceBuff[counter] = max;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	/**
	 * Calculates the minimum between 2 images
	 * @param xytBuff
	 * @return
	 */
	private byte[] doByteMin(byte[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		byte[] sliceBuff = new byte[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			byte min = 0;
			for(int t=0;t<tDim;t++) {
				byte pix = xytBuff[i + (sliceLength * t)];
				if(t == 0) {
					min = pix;
				}
				if(pix < min) {
					min = pix;
				}
				
				
				
			}

			sliceBuff[counter] = min;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	/**
	 * Calculates the average between 2 images
	 * @param xytBuff
	 * @return
	 */
	private byte[] doByteAverage(byte[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		byte[] sliceBuff = new byte[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			for(int t=0;t<tDim;t++) {
				byte pix = xytBuff[i + (sliceLength * t)];

				sum = (sum + pix);
				
				
			}

			
			byte average = (byte)Math.round(sum/tDim);
			sliceBuff[counter] = average;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	/**
	 * Calculates the norm between 2 images
	 * @param xytBuff
	 * @return
	 */
	private byte[] doByteNormClip(byte[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		byte[] sliceBuff = new byte[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			for(int t=0;t<tDim;t++) {
				byte pix = xytBuff[i + (sliceLength * t)];
				int pixSquared = pix * pix;

				sum = (sum + pixSquared);
				
				
			}

			
	
			double n = Math.sqrt(sum);
			if(n > Byte.MAX_VALUE) {
				n = Byte.MAX_VALUE;
			}

			byte norm = (byte)n;
			sliceBuff[counter] = norm;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	
	/**
	 * Calculates the norm between 2 images
	 * @param xytBuff
	 * @return
	 */
	private short[] doByteNormPromote(byte[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		short[] sliceBuff = new short[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			for(int t=0;t<tDim;t++) {
				byte pix = xytBuff[i + (sliceLength * t)];
				int pixSquared = pix * pix;

				sum = (sum + pixSquared);
				
				
			}

			
	
			double n = Math.sqrt(sum);


			short norm = (short)n;
			sliceBuff[counter] = norm;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	
	
	/**
	 * Calculates the standard deviation between 2 images
	 * @param xytBuff
	 * @return
	 */
	private byte[] doByteStdDev(byte[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		double[] sliceBuff = new double[xDim * yDim];
		int counter = 0;
		//first do averaging
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			for(int t=0;t<tDim;t++) {
				byte pix = xytBuff[i + (sliceLength * t)];
				sum = (sum + pix);
			}

			double average = (sum/tDim);
			sliceBuff[counter] = average;
			counter++;
		}
		
		
		//now calculate std dev
		counter = 0;
		byte[] sliceStdDevBuff = new byte[xDim * yDim];
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			double average = sliceBuff[i];
			for(int t=0;t<tDim;t++) {
				byte pix = xytBuff[i + (sliceLength * t)];
				double calc = (pix-average) * (pix-average);
				sum = sum + calc;
			}

			byte stddev = (byte)(Math.sqrt(sum/tDim));
			sliceStdDevBuff[counter] = stddev;
			counter++;
			
		}
		

		return sliceStdDevBuff;
	}
	
	/**
	 * Calculates addition between 2 images
	 * @param xytBuff
	 * @return
	 */
	private byte[] doByteAddClip(byte[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		byte[] sliceBuff = new byte[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			int sum = 0;
			for(int t=0;t<tDim;t++) {
				byte pix = xytBuff[i + (sliceLength * t)];

				sum = (sum + pix);
				
				
				
			}
			
			if(sum > Byte.MAX_VALUE) {
				sum = Byte.MAX_VALUE;
			}
			if (sum < Byte.MIN_VALUE) {
				sum = Byte.MIN_VALUE;
			}

			sliceBuff[counter] = (byte)sum;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	
	
	/**
	 * Calculates addition between 2 images
	 * @param xytBuff
	 * @return
	 */
	private short[] doByteAddPromote(byte[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		short[] sliceBuff = new short[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			int sum = 0;
			for(int t=0;t<tDim;t++) {
				byte pix = xytBuff[i + (sliceLength * t)];

				sum = (sum + pix);
				
				
				
			}
			

			sliceBuff[counter] = (short)sum;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	
	
	
	/**
	 * Calculates the max between 2 images
	 * @param xytBuff
	 * @return
	 */
	private short[] doShortMax(short[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		short[] sliceBuff = new short[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			short max = 0;
			for(int t=0;t<tDim;t++) {
				short pix = xytBuff[i + (sliceLength * t)];
				if(t == 0) {
					max = pix;
				}
				if(pix > max) {
					max = pix;
				}
				
				
				
			}

			sliceBuff[counter] = max;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	/**
	 * Calculates the minimum between 2 images
	 * @param xytBuff
	 * @return
	 */
	private short[] doShortMin(short[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		short[] sliceBuff = new short[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			short min = 0;
			for(int t=0;t<tDim;t++) {
				short pix = xytBuff[i + (sliceLength * t)];
				if(t == 0) {
					min = pix;
				}
				if(pix < min) {
					min = pix;
				}
				
				
				
			}

			sliceBuff[counter] = min;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	/**
	 * Calculates the average between 2 images
	 * @param xytBuff
	 * @return
	 */
	private short[] doShortAverage(short[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		short[] sliceBuff = new short[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			for(int t=0;t<tDim;t++) {
				short pix = xytBuff[i + (sliceLength * t)];

				sum = (sum + pix);

				
			}

			
			short average = (short)Math.round(sum/tDim);
			sliceBuff[counter] = average;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	/**
	 * Calculates the norm between 2 images
	 * @param xytBuff
	 * @return
	 */
	private short[] doShortNormClip(short[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		short[] sliceBuff = new short[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			for(int t=0;t<tDim;t++) {
				short pix = xytBuff[i + (sliceLength * t)];
				int pixSquared = pix * pix;

				sum = (sum + pixSquared);
				
				
			}

			
	
			double d = Math.sqrt(sum);
			
			
			if (dataType == ModelStorageBase.SHORT) {
				if(d > Short.MAX_VALUE) {
					d = Short.MAX_VALUE;
				}
			}
			else if (dataType == ModelStorageBase.UBYTE) {
				if (d > 255) {
					d = 255;
				}
			}
			short norm = (short)d;
			sliceBuff[counter] = norm;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	/**
	 * Calculates the norm between 2 short images
	 * @param xytBuff - the xyt
	 * @return
	 */
	private int[] doShortNormPromote(short[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		int[] sliceBuff = new int[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			for(int t=0;t<tDim;t++) {
				short pix = xytBuff[i + (sliceLength * t)];
				int pixSquared = pix * pix;

				sum = (sum + pixSquared);
				
				
			}

			
	
			double d = Math.sqrt(sum);
			
			

			int norm = (int)d;
			sliceBuff[counter] = norm;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	
	/**
	 * Calculates the standard deviation between 2 images
	 * @param xytBuff
	 * @return
	 */
	private short[] doShortStdDev(short[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		double[] sliceBuff = new double[xDim * yDim];
		int counter = 0;
		//first do averaging
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			for(int t=0;t<tDim;t++) {
				short pix = xytBuff[i + (sliceLength * t)];
				sum = (sum + pix);
			}

			double average = (sum/tDim);
			sliceBuff[counter] = average;
			counter++;
		}
		
		
		//now calculate std dev
		counter = 0;
		short[] sliceStdDevBuff = new short[xDim * yDim];
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			double average = sliceBuff[i];
			for(int t=0;t<tDim;t++) {
				short pix = xytBuff[i + (sliceLength * t)];
				double calc = (pix-average) * (pix-average);
				sum = sum + calc;
			}

			short stddev = (short)(Math.sqrt(sum/tDim));
			sliceStdDevBuff[counter] = stddev;
			counter++;
			
		}
		

		return sliceStdDevBuff;
	}
	
	
	
	
	
	
	
	/**
	 * Calculates addition between 2 images
	 * @param xytBuff
	 * @return
	 */
	private short[] doShortAddClip(short[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		short[] sliceBuff = new short[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			int sum = 0;
			for(int t=0;t<tDim;t++) {
				short pix = xytBuff[i + (sliceLength * t)];

				sum = (sum + pix);
				
				
				
			}
			
			if(dataType == ModelStorageBase.SHORT) {
				if(sum > Short.MAX_VALUE) {
					sum = Short.MAX_VALUE;
				}
				else if (sum < Short.MIN_VALUE) {
					sum = Short.MIN_VALUE;
				}
			}else if(dataType == ModelStorageBase.UBYTE) {
				if(sum > 255) {
					sum = 255;
				}
				else if (sum < 0) {
					sum = 0;
				}
			}
			

			sliceBuff[counter] = (short)sum;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	/**
	 * Calculates addition between 2 images
	 * @param xytBuff
	 * @return
	 */
	private int[] doShortAddPromote(short[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		int[] sliceBuff = new int[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			int sum = 0;
			for(int t=0;t<tDim;t++) {
				short pix = xytBuff[i + (sliceLength * t)];

				sum = (sum + pix);
				
				
				
			}
			


			sliceBuff[counter] = sum;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	/**
	 * Calculates the max between 2 images
	 * @param xytBuff
	 * @return
	 */
	private int[] doIntMax(int[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		int[] sliceBuff = new int[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			int max = 0;
			for(int t=0;t<tDim;t++) {
				int pix = xytBuff[i + (sliceLength * t)];
				if(t == 0) {
					max = pix;
				}
				if(pix > max) {
					max = pix;
				}
				
				
				
			}

			sliceBuff[counter] = max;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	/**
	 * Calculates the minimum between 2 images
	 * @param xytBuff
	 * @return
	 */
	private int[] doIntMin(int[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		int[] sliceBuff = new int[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			int min = 0;
			for(int t=0;t<tDim;t++) {
				int pix = xytBuff[i + (sliceLength * t)];
				if(t == 0) {
					min = pix;
				}
				if(pix < min) {
					min = pix;
				}
				
				
				
			}

			sliceBuff[counter] = min;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	/**
	 * Calculates the average between 2 images
	 * @param xytBuff
	 * @return
	 */
	private int[] doIntAverage(int[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		int[] sliceBuff = new int[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			for(int t=0;t<tDim;t++) {
				int pix = xytBuff[i + (sliceLength * t)];

				sum = (sum + pix);

				
			}

			
			double average = (sum/tDim);
			sliceBuff[counter] = (int)Math.round(average);
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	/**
	 * Calculates the norm between 2 images
	 * @param xytBuff
	 * @return
	 */
	private int[] doIntNormClip(int[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		int[] sliceBuff = new int[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			for(int t=0;t<tDim;t++) {
				int pix = xytBuff[i + (sliceLength * t)];
				long pixSquared = pix * pix;

				sum = (sum + pixSquared);
				
				
			}

			
			double d = Math.sqrt(sum);
			
			if (dataType == ModelStorageBase.INTEGER) {
				if(d > Integer.MAX_VALUE) {
					d = Integer.MAX_VALUE;
				}
			}
			else if (dataType == ModelStorageBase.USHORT) {
				if (d > 65535) {
					d = 65535;
				}
			}
			
			int norm = (int)d;
			sliceBuff[counter] = norm;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	/**
	 * Calculates the norm between 2 images
	 * @param xytBuff
	 * @return
	 */
	private long[] doIntNormPromote(int[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		long[] sliceBuff = new long[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			for(int t=0;t<tDim;t++) {
				int pix = xytBuff[i + (sliceLength * t)];
				long pixSquared = pix * pix;

				sum = (sum + pixSquared);
				
				
			}

			
			double d = Math.sqrt(sum);
			

			
			long norm = (long)d;
			sliceBuff[counter] = norm;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	
	
	/**
	 * Calculates the standard deviation between 2 images
	 * @param xytBuff
	 * @return
	 */
	private int[] doIntStdDev(int[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		double[] sliceBuff = new double[xDim * yDim];
		int counter = 0;
		//first do averaging
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			for(int t=0;t<tDim;t++) {
				int pix = xytBuff[i + (sliceLength * t)];
				sum = (sum + pix);
			}

			double average = (sum/tDim);
			sliceBuff[counter] = average;
			counter++;
		}
		
		
		//now calculate std dev
		counter = 0;
		int[] sliceStdDevBuff = new int[xDim * yDim];
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			double average = sliceBuff[i];
			for(int t=0;t<tDim;t++) {
				int pix = xytBuff[i + (sliceLength * t)];
				double calc = (pix-average) * (pix-average);
				sum = sum + calc;
			}

			double stddev = (Math.sqrt(sum/tDim));
			sliceStdDevBuff[counter] = (int)Math.round(stddev);
			counter++;
			
		}
		

		return sliceStdDevBuff;
	}
	
	
	
	/**
	 * Calculates addition between 2 images
	 * @param xytBuff
	 * @return
	 */
	private int[] doIntAddClip(int[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		int[] sliceBuff = new int[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			for(int t=0;t<tDim;t++) {
				int pix = xytBuff[i + (sliceLength * t)];

				sum = (sum + pix);
				
				
				
			}
			
			if(dataType == ModelStorageBase.INTEGER) {
				if(sum > Integer.MAX_VALUE) {
					sum = Integer.MAX_VALUE;
				}
				else if (sum < Integer.MIN_VALUE) {
					sum = Integer.MIN_VALUE;
				}
			}else if(dataType == ModelStorageBase.USHORT) {
				if(sum > 65535) {
					sum = 65535;
				} else if (sum < 0) {
					sum = 0;
				}
			}
			
			

			sliceBuff[counter] = (int)sum;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	/**
	 * Calculates addition between 2 images
	 * @param xytBuff
	 * @return
	 */
	private long[] doIntAddPromote(int[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		long[] sliceBuff = new long[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			for(int t=0;t<tDim;t++) {
				int pix = xytBuff[i + (sliceLength * t)];

				sum = (sum + pix);
				
				
				
			}
			

			sliceBuff[counter] = (long)sum;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	/**
	 * Calculates the max between 2 images
	 * @param xytBuff
	 * @return
	 */
	private float[] doFloatMax(float[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		float[] sliceBuff = new float[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			float max = 0;
			for(int t=0;t<tDim;t++) {
				float pix = xytBuff[i + (sliceLength * t)];
				if(t == 0) {
					max = pix;
				}
				if(pix > max) {
					max = pix;
				}
				
				
				
			}

			sliceBuff[counter] = max;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	/**
	 * Calculates the minimum between 2 images
	 * @param xytBuff
	 * @return
	 */
	private float[] doFloatMin(float[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		float[] sliceBuff = new float[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			float min = 0;
			for(int t=0;t<tDim;t++) {
				float pix = xytBuff[i + (sliceLength * t)];
				if(t == 0) {
					min = pix;
				}
				if(pix < min) {
					min = pix;
				}
				
				
				
			}

			sliceBuff[counter] = min;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	/**
	 * Calculates the average between 2 images
	 * @param xytBuff
	 * @return
	 */
	private float[] doFloatAverage(float[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		float[] sliceBuff = new float[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			for(int t=0;t<tDim;t++) {
				float pix = xytBuff[i + (sliceLength * t)];

				sum = (sum + pix);

				
			}

			
			float average = (float)(sum/tDim);
			sliceBuff[counter] = average;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	
	/**
	 * Calculates the norm between 2 images
	 * @param xytBuff
	 * @return
	 */
	private float[] doFloatNormClip(float[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		float[] sliceBuff = new float[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			for(int t=0;t<tDim;t++) {
				float pix = xytBuff[i + (sliceLength * t)];
				double pixSquared = pix * pix;

				sum = (sum + pixSquared);
				
				
			}

			
			double d = Math.sqrt(sum);
			
			if(d > Float.MAX_VALUE) {
				d = Float.MAX_VALUE;
			}
	
			
			float norm = (float)d;
			sliceBuff[counter] = norm;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	/**
	 * Calculates the norm between 2 images
	 * @param xytBuff
	 * @return
	 */
	private double[] doFloatNormPromote(float[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		double[] sliceBuff = new double[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			for(int t=0;t<tDim;t++) {
				float pix = xytBuff[i + (sliceLength * t)];
				double pixSquared = pix * pix;

				sum = (sum + pixSquared);
				
				
			}

			
			double d = Math.sqrt(sum);
			

			sliceBuff[counter] = d;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	/**
	 * Calculates the standard deviation between 2 images
	 * @param xytBuff
	 * @return
	 */
	private float[] doFloatStdDev(float[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		double[] sliceBuff = new double[xDim * yDim];
		int counter = 0;
		//first do averaging
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			for(int t=0;t<tDim;t++) {
				float pix = xytBuff[i + (sliceLength * t)];
				sum = (sum + pix);
			}

			double average = (sum/tDim);
			sliceBuff[counter] = average;
			counter++;
		}
		
		
		//now calculate std dev
		counter = 0;
		float[] sliceStdDevBuff = new float[xDim * yDim];
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			double average = sliceBuff[i];
			for(int t=0;t<tDim;t++) {
				float pix = xytBuff[i + (sliceLength * t)];
				double calc = (pix-average) * (pix-average);
				sum = sum + calc;
			}

			float stddev = (float)(Math.sqrt(sum/tDim));
			sliceStdDevBuff[counter] = stddev;
			counter++;
			
		}
		

		return sliceStdDevBuff;
	}
	
	/**
	 * Calculates addition between 2 images
	 * @param xytBuff
	 * @return
	 */
	private float[] doFloatAddClip(float[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		float[] sliceBuff = new float[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			for(int t=0;t<tDim;t++) {
				float pix = xytBuff[i + (sliceLength * t)];

				sum = (sum + pix);
				
				
				
			}
			
			if(sum > Float.MAX_VALUE) {
				sum = Float.MAX_VALUE;
			} else if (sum < -Float.MAX_VALUE) {
				sum = -Float.MAX_VALUE;
			}

			sliceBuff[counter] = (float)sum;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	
	
	/**
	 * Calculates addition between 2 images
	 * @param xytBuff
	 * @return
	 */
	private double[] doFloatAddPromote(float[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		double[] sliceBuff = new double[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			for(int t=0;t<tDim;t++) {
				float pix = xytBuff[i + (sliceLength * t)];

				sum = (sum + pix);
				
				
				
			}
			

			sliceBuff[counter] = sum;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	/**
	 * Calculates addition between 2 images
	 * @param xytBuff
	 * @return
	 */
	private long[] doLongAddClip(long[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		long[] sliceBuff = new long[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			for(int t=0;t<tDim;t++) {
				long pix = xytBuff[i + (sliceLength * t)];

				sum = (sum + pix);
				
				
				
			}
			
			if(dataType == ModelStorageBase.LONG) {
				if(sum > Long.MAX_VALUE) {
					sum = Long.MAX_VALUE;
				}
				else if (sum < Long.MIN_VALUE) {
					sum = Long.MIN_VALUE;
				}
			}else if(dataType == ModelStorageBase.UINTEGER) {
				if(sum > 4294967295L) {
					sum = 4294967295L;
				}
				else if (sum < 0) {
					sum = 0L;
				}
			}
			
			

			sliceBuff[counter] = (long)sum;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	/**
	 * Calculates the average between 2 images
	 * @param xytBuff
	 * @return
	 */
	private long[] doLongAverage(long[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		long[] sliceBuff = new long[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			for(int t=0;t<tDim;t++) {
				long pix = xytBuff[i + (sliceLength * t)];

				sum = (sum + pix);

				
			}

			
			double average = (sum/tDim);
			sliceBuff[counter] = Math.round(average);
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	/**
	 * Calculates the max between 2 images
	 * @param xytBuff
	 * @return
	 */
	private long[] doLongMax(long[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		long[] sliceBuff = new long[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			long max = 0L;
			for(int t=0;t<tDim;t++) {
				long pix = xytBuff[i + (sliceLength * t)];
				if(t == 0) {
					max = pix;
				}
				if(pix > max) {
					max = pix;
				}
				
				
				
			}

			sliceBuff[counter] = max;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	/**
	 * Calculates the minimum between 2 images
	 * @param xytBuff
	 * @return
	 */
	private long[] doLongMin(long[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		long[] sliceBuff = new long[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			long min = 0L;
			for(int t=0;t<tDim;t++) {
				long pix = xytBuff[i + (sliceLength * t)];
				if(t == 0) {
					min = pix;
				}
				if(pix < min) {
					min = pix;
				}
				
				
				
			}

			sliceBuff[counter] = min;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	/**
	 * Calculates the standard deviation between 2 images
	 * @param xytBuff
	 * @return
	 */
	private long[] doLongStdDev(long[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		double[] sliceBuff = new double[xDim * yDim];
		int counter = 0;
		//first do averaging
		for(int i=0;i<sliceLength;i++) {
			double sum = 0L;
			for(int t=0;t<tDim;t++) {
				long pix = xytBuff[i + (sliceLength * t)];
				sum = (sum + pix);
			}

			double average = (sum/tDim);
			sliceBuff[counter] = average;
			counter++;
		}
		
		
		//now calculate std dev
		counter = 0;
		long[] sliceStdDevBuff = new long[xDim * yDim];
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			double average = sliceBuff[i];
			for(int t=0;t<tDim;t++) {
				double pix = xytBuff[i + (sliceLength * t)];
				double calc = (pix-average) * (pix-average);
				sum = sum + calc;
			}

			double stddev = (Math.sqrt(sum/tDim));
			sliceStdDevBuff[counter] = Math.round(stddev);
			counter++;
			
		}
		
		return sliceStdDevBuff;
	}
	
	/**
	 * Calculates the norm between 2 images
	 * @param xytBuff
	 * @return
	 */
	private long[] doLongNormClip(long[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		long[] sliceBuff = new long[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			for(int t=0;t<tDim;t++) {
				double pix = xytBuff[i + (sliceLength * t)];
				double pixSquared = pix * pix;

				sum = (sum + pixSquared);
				
				
			}

			
			double d = Math.sqrt(sum);
			
			if(d > Long.MAX_VALUE) {
				d = Long.MAX_VALUE;
			}
			
			long norm = Math.round(d);
			sliceBuff[counter] = norm;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	/**
	 * Calculates addition between 2 images
	 * @param xytBuff
	 * @return
	 */
	private double[] doDoubleAddClip(double[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		double[] sliceBuff = new double[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			for(int t=0;t<tDim;t++) {
				double pix = xytBuff[i + (sliceLength * t)];

				sum = (sum + pix);
				
				
				
			}
			
			if(sum > Double.MAX_VALUE) {
				sum = Double.MAX_VALUE;
			} else if (sum < -Double.MAX_VALUE) {
				sum = -Double.MAX_VALUE;
			}

			sliceBuff[counter] = sum;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	/**
	 * Calculates the average between 2 images
	 * @param xytBuff
	 * @return
	 */
	private double[] doDoubleAverage(double[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		double[] sliceBuff = new double[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			for(int t=0;t<tDim;t++) {
				double pix = xytBuff[i + (sliceLength * t)];

				sum = (sum + pix);

				
			}
			
			sliceBuff[counter] = sum/tDim;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	/**
	 * Calculates the max between 2 images
	 * @param xytBuff
	 * @return
	 */
	private double[] doDoubleMax(double[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		double[] sliceBuff = new double[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			double max = 0;
			for(int t=0;t<tDim;t++) {
				double pix = xytBuff[i + (sliceLength * t)];
				if(t == 0) {
					max = pix;
				}
				if(pix > max) {
					max = pix;
				}
				
				
				
			}

			sliceBuff[counter] = max;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	/**
	 * Calculates the minimum between 2 images
	 * @param xytBuff
	 * @return
	 */
	private double[] doDoubleMin(double[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		double[] sliceBuff = new double[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			double min = 0;
			for(int t=0;t<tDim;t++) {
				double pix = xytBuff[i + (sliceLength * t)];
				if(t == 0) {
					min = pix;
				}
				if(pix < min) {
					min = pix;
				}
				
				
				
			}

			sliceBuff[counter] = min;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	/**
	 * Calculates the standard deviation between 2 images
	 * @param xytBuff
	 * @return
	 */
	private double[] doDoubleStdDev(double[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		double[] sliceBuff = new double[xDim * yDim];
		int counter = 0;
		//first do averaging
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			for(int t=0;t<tDim;t++) {
				double pix = xytBuff[i + (sliceLength * t)];
				sum = (sum + pix);
			}

			double average = (sum/tDim);
			sliceBuff[counter] = average;
			counter++;
		}
		
		
		//now calculate std dev
		counter = 0;
		double[] sliceStdDevBuff = new double[xDim * yDim];
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			double average = sliceBuff[i];
			for(int t=0;t<tDim;t++) {
				double pix = xytBuff[i + (sliceLength * t)];
				double calc = (pix-average) * (pix-average);
				sum = sum + calc;
			}

			sliceStdDevBuff[counter] = (Math.sqrt(sum/tDim));
			counter++;
			
		}
		

		return sliceStdDevBuff;
	}
	
	/**
	 * Calculates the norm between 2 images
	 * @param xytBuff
	 * @return
	 */
	private double[] doDoubleNormClip(double[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		double[] sliceBuff = new double[xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i++) {
			double sum = 0;
			for(int t=0;t<tDim;t++) {
				double pix = xytBuff[i + (sliceLength * t)];
				double pixSquared = pix * pix;

				sum = (sum + pixSquared);
				
				
			}

			
			double d = Math.sqrt(sum);
			
			if(d > Double.MAX_VALUE) {
				d = Double.MAX_VALUE;
			}
	
			
			sliceBuff[counter] = d;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	/**
	 * Calculates addition between 2 images
	 * @param xytBuff
	 * @return
	 */
	private short[] doShortAddClipARGB(short[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = 4 * xDim * yDim;
		short[] sliceBuff = new short[4 * xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i+= 4) {
			int sumA = 0;
			int sumR = 0;
			int sumG = 0;
			int sumB = 0;
			for(int t=0;t<tDim;t++) {
				short pixA = xytBuff[i + (sliceLength * t)];
				short pixR = xytBuff[i + 1 + (sliceLength * t)];
				short pixG = xytBuff[i + 2 + (sliceLength * t)];
				short pixB = xytBuff[i + 3 + (sliceLength * t)];

				sumA = (sumA + pixA);
				sumR = (sumR + pixR);
				sumG = (sumG + pixG);
				sumB = (sumB + pixB);
				
			}
			
			if(sumA > 255) {
				sumA = 255;
			}
			if (sumR > 255) {
				sumR = 255;
			}
			if (sumG > 255) {
				sumG = 255;
			}
			if (sumB > 255) {
				sumB = 255;
			}

			sliceBuff[counter++] = (short)sumA;
			sliceBuff[counter++] = (short)sumR;
			sliceBuff[counter++] = (short)sumG;
			sliceBuff[counter++] = (short)sumB;
			
		}
	    
		return sliceBuff;
	}
	
	/**
	 * Calculates the average between 2 images
	 * @param xytBuff
	 * @return
	 */
	private short[] doShortAverageARGB(short[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = 4 * xDim * yDim;
		short[] sliceBuff = new short[4 * xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i+=4) {
			double sumA = 0;
			double sumR = 0;
			double sumG = 0;
			double sumB = 0;
			for(int t=0;t<tDim;t++) {
				short pixA = xytBuff[i + (sliceLength * t)];
				short pixR = xytBuff[i + 1 + (sliceLength * t)];
				short pixG = xytBuff[i + 2 + (sliceLength * t)];
				short pixB = xytBuff[i + 3 + (sliceLength * t)];

				sumA = (sumA + pixA);
				sumR = (sumR + pixR);
				sumG = (sumG + pixG);
				sumB = (sumB + pixB);
				
			}

			
			short averageA = (short)Math.round(sumA/tDim);
			sliceBuff[counter++] = averageA;
			short averageR = (short)Math.round(sumR/tDim);
			sliceBuff[counter++] = averageR;
			short averageG = (short)Math.round(sumG/tDim);
			sliceBuff[counter++] = averageG;
			short averageB = (short)Math.round(sumB/tDim);
			sliceBuff[counter++] = averageB;
		}
		

		return sliceBuff;
	}
	
	/**
	 * Calculates the max between 2 images
	 * @param xytBuff
	 * @return
	 */
	private short[] doShortMaxARGB(short[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = 4 * xDim * yDim;
		short[] sliceBuff = new short[4 * xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i+=4) {
			short maxA = 0;
			short maxR = 0;
			short maxG = 0;
			short maxB = 0;
			for(int t=0;t<tDim;t++) {
				short pixA = xytBuff[i + (sliceLength * t)];
				short pixR = xytBuff[i + 1 + (sliceLength * t)];
				short pixG = xytBuff[i + 2 + (sliceLength * t)];
				short pixB = xytBuff[i + 3 + (sliceLength * t)];
				
				if(t == 0) {
					maxA = pixA;
					maxR = pixR;
					maxG = pixG;
					maxB = pixB;
				}
				if(pixA > maxA) {
					maxA = pixA;
				}
				if (pixR > maxR) {
					maxR = pixR;
				}
				if (pixG > maxG) {
					maxG = pixG;
				}
				if (pixB > maxB){
				    maxB = pixB;	
				}
				
				
			}

			sliceBuff[counter++] = maxA;
			sliceBuff[counter++] = maxR;
			sliceBuff[counter++] = maxG;
			sliceBuff[counter++] = maxB;
		}
		

		return sliceBuff;
	}
	
	/**
	 * Calculates the minimum between 2 images
	 * @param xytBuff
	 * @return
	 */
	private short[] doShortMinARGB(short[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = 4 * xDim * yDim;
		short[] sliceBuff = new short[4 * xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i+=4) {
			short minA = 0;
			short minR = 0;
			short minG = 0;
			short minB = 0;
			for(int t=0;t<tDim;t++) {
				short pixA = xytBuff[i + (sliceLength * t)];
				short pixR = xytBuff[i + 1 + (sliceLength * t)];
				short pixG = xytBuff[i + 2 + (sliceLength * t)];
				short pixB = xytBuff[i + 3 + (sliceLength * t)];
				if(t == 0) {
					minA = pixA;
					minR = pixR;
					minG = pixG;
					minB = pixB;
				}
				if(pixA < minA) {
					minA = pixA;
				}
				if(pixR < minR) {
					minR = pixR;
				}
				if(pixG < minG) {
					minG = pixG;
				}
				if(pixB < minB) {
					minB = pixB;
				}
				
			}

			sliceBuff[counter++] = minA;
			sliceBuff[counter++] = minR;
			sliceBuff[counter++] = minG;
			sliceBuff[counter++] = minB;
			
		}
		

		return sliceBuff;
	}
	
	/**
	 * Calculates the standard deviation between 2 images
	 * @param xytBuff
	 * @return
	 */
	private short[] doShortStdDevARGB(short[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = 4 * xDim * yDim;
		double[] sliceBuff = new double[4 * xDim * yDim];
		int counter = 0;
		//first do averaging
		for(int i=0;i<sliceLength;i+=4) {
			double sumA = 0;
			double sumR = 0;
			double sumG = 0;
			double sumB = 0;
			for(int t=0;t<tDim;t++) {
				short pixA = xytBuff[i + (sliceLength * t)];
				short pixR = xytBuff[i + 1 + (sliceLength * t)];
				short pixG = xytBuff[i + 2 + (sliceLength * t)];
				short pixB = xytBuff[i + 3 + (sliceLength * t)];
				sumA = (sumA + pixA);
				sumR = (sumR + pixR);
				sumG = (sumG + pixG);
				sumB = (sumB + pixB);
			}

			sliceBuff[counter++] = (sumA/tDim);
			sliceBuff[counter++] = (sumR/tDim);
			sliceBuff[counter++] = (sumG/tDim);
			sliceBuff[counter++] = (sumB/tDim);
		}
		
		
		//now calculate std dev
		counter = 0;
		short[] sliceStdDevBuff = new short[4 * xDim * yDim];
		for(int i=0;i<sliceLength;i+=4) {
			double sumA = 0;
			double sumR = 0;
			double sumG = 0;
			double sumB = 0;
			double averageA = sliceBuff[i];
			double averageR = sliceBuff[i+1];
			double averageG = sliceBuff[i+2];
			double averageB = sliceBuff[i+3];
			for(int t=0;t<tDim;t++) {
				short pixA = xytBuff[i + (sliceLength * t)];
				short pixR = xytBuff[i + 1 + (sliceLength * t)];
				short pixG = xytBuff[i + 2 + (sliceLength * t)];
				short pixB = xytBuff[i + 3 + (sliceLength * t)];
				double calcA = (pixA-averageA) * (pixA-averageA);
				double calcR = (pixR-averageR) * (pixR-averageR);
				double calcG = (pixG-averageG) * (pixG-averageG);
				double calcB = (pixB-averageB) * (pixB-averageB);
				sumA = sumA + calcA;
				sumR = sumR + calcR;
				sumG = sumG + calcG;
				sumB = sumB + calcB;
			}

			short stddevA = (short)(Math.sqrt(sumA/tDim));
			short stddevR = (short)(Math.sqrt(sumR/tDim));
			short stddevG = (short)(Math.sqrt(sumG/tDim));
			short stddevB = (short)(Math.sqrt(sumB/tDim));
			sliceStdDevBuff[counter++] = stddevA;
			sliceStdDevBuff[counter++] = stddevR;
			sliceStdDevBuff[counter++] = stddevG;
			sliceStdDevBuff[counter++] = stddevB;
		}
		

		return sliceStdDevBuff;
	}
	
	/**
	 * Calculates the norm between 2 images
	 * @param xytBuff
	 * @return
	 */
	private short[] doShortNormClipARGB(short[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = 4 * xDim * yDim;
		short[] sliceBuff = new short[4 * xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i+=4) {
			double sumA = 0;
			double sumR = 0;
			double sumG = 0;
			double sumB = 0;
			for(int t=0;t<tDim;t++) {
				short pixA = xytBuff[i + (sliceLength * t)];
				short pixR = xytBuff[i + 1 + (sliceLength * t)];
				short pixG = xytBuff[i + 2 + (sliceLength * t)];
				short pixB = xytBuff[i + 3 + (sliceLength * t)];
				int pixSquaredA = pixA * pixA;
				int pixSquaredR = pixR * pixR;
				int pixSquaredG = pixG * pixG;
				int pixSquaredB = pixB * pixB;

				sumA = (sumA + pixSquaredA);
				sumR = (sumR + pixSquaredR);
				sumG = (sumG + pixSquaredG);
				sumB = (sumB + pixSquaredB);
			}

			
	
			double dA = Math.sqrt(sumA);
			double dR = Math.sqrt(sumR);
			double dG = Math.sqrt(sumG);
			double dB = Math.sqrt(sumB);
			
			
			if(dA > 255) {
				dA = 255;
			}
			if(dR > 255) {
				dR = 255;
			}
			if(dG > 255) {
				dG = 255;
			}
			if(dB > 255) {
				dB = 255;
			}
			short normA = (short)dA;
			short normR = (short)dR;
			short normG = (short)dG;
			short normB = (short)dB;
			sliceBuff[counter++] = normA;
			sliceBuff[counter++] = normR;
			sliceBuff[counter++] = normG;
			sliceBuff[counter++] = normB;
		}
		

		return sliceBuff;
	}
	
	/**
	 * Calculates addition between 2 images
	 * @param xytBuff
	 * @return
	 */
	private int[] doShortAddPromoteARGB(short[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = 4 * xDim * yDim;
		int[] sliceBuff = new int[4 * xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i+=4) {
			int sumA = 0;
			int sumR = 0;
			int sumG = 0;
			int sumB = 0;
			for(int t=0;t<tDim;t++) {
				short pixA = xytBuff[i + (sliceLength * t)];
				short pixR = xytBuff[i + 1 + (sliceLength * t)];
				short pixG = xytBuff[i + 2 + (sliceLength * t)];
				short pixB = xytBuff[i + 3 + (sliceLength * t)];

				sumA = (sumA + pixA);
				sumR = (sumR + pixR);
				sumG = (sumG + pixG);
				sumB = (sumB + pixB);
			}
			


			sliceBuff[counter++] = sumA;
			sliceBuff[counter++] = sumR;
			sliceBuff[counter++] = sumG;
			sliceBuff[counter++] = sumB;
		}
		

		return sliceBuff;
	}
	
	/**
	 * Calculates the norm between 2 short images
	 * @param xytBuff - the xyt
	 * @return
	 */
	private int[] doShortNormPromoteARGB(short[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = 4 * xDim * yDim;
		int[] sliceBuff = new int[4 * xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i+=4) {
			double sumA = 0;
			double sumR = 0;
			double sumG = 0;
			double sumB = 0;
			
			for(int t=0;t<tDim;t++) {
				short pixA = xytBuff[i + (sliceLength * t)];
				short pixR = xytBuff[i + 1 + (sliceLength * t)];
				short pixG = xytBuff[i + 2 + (sliceLength * t)];
				short pixB = xytBuff[i + 3 + (sliceLength * t)];
				int pixSquaredA = pixA * pixA;
				int pixSquaredR = pixR * pixR;
				int pixSquaredG = pixG * pixG;
				int pixSquaredB = pixB * pixB;

				sumA = (sumA+ pixSquaredA);
				sumR = (sumR+ pixSquaredR);
				sumG = (sumG+ pixSquaredG);
				sumB = (sumB+ pixSquaredB);
			}

			
	
			double dA = Math.sqrt(sumA);
			double dR = Math.sqrt(sumR);
			double dG = Math.sqrt(sumG);
			double dB = Math.sqrt(sumB);

			int normA = (int)dA;
			int normR = (int)dR;
			int normG = (int)dG;
			int normB = (int)dB;
			sliceBuff[counter++] = normA;
			sliceBuff[counter++] = normR;
			sliceBuff[counter++] = normG;
			sliceBuff[counter++] = normB;
		}
		

		return sliceBuff;
	}
	
	/**
	 * Calculates addition between 2 images
	 * @param xytBuff
	 * @return
	 */
	private int[] doIntAddClipARGB(int[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = 4 * xDim * yDim;
		int[] sliceBuff = new int[4 * xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i+=4) {
			double sumA = 0;
			double sumR = 0;
			double sumG = 0;
			double sumB = 0;
			for(int t=0;t<tDim;t++) {
				int pixA = xytBuff[i + (sliceLength * t)];
				int pixR = xytBuff[i + 1 + (sliceLength * t)];
				int pixG = xytBuff[i + 2 + (sliceLength * t)];
				int pixB = xytBuff[i + 3 + (sliceLength * t)];

				sumA = (sumA + pixA);
				sumR = (sumR + pixR);
				sumG = (sumG + pixG);
				sumB = (sumB + pixB);
			}
			
			
			if(sumA > 65535) {
				sumA = 65535;
			} 
			if(sumR > 65535) {
				sumR = 65535;
			} 
			if(sumG > 65535) {
				sumG = 65535;
			} 
			if(sumB > 65535) {
				sumB = 65535;
			} 
			

			sliceBuff[counter++] = (int)sumA;
			sliceBuff[counter++] = (int)sumR;
			sliceBuff[counter++] = (int)sumG;
			sliceBuff[counter++] = (int)sumB;
		}
		

		return sliceBuff;
	}
	
	/**
	 * Calculates the average between 2 images
	 * @param xytBuff
	 * @return
	 */
	private int[] doIntAverageARGB(int[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = 4 * xDim * yDim;
		int[] sliceBuff = new int[4 * xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i+=4) {
			double sumA = 0;
			double sumR = 0;
			double sumG = 0;
			double sumB = 0;
			for(int t=0;t<tDim;t++) {
				int pixA = xytBuff[i + (sliceLength * t)];
				int pixR = xytBuff[i + 1 + (sliceLength * t)];
				int pixG = xytBuff[i + 2 + (sliceLength * t)];
				int pixB = xytBuff[i + 3 + (sliceLength * t)];

				sumA = (sumA + pixA);
				sumR = (sumR + pixR);
				sumG = (sumG + pixG);
				sumB = (sumB + pixB);
			}

			
			double averageA = (sumA/tDim);
			double averageR = (sumR/tDim);
			double averageG = (sumG/tDim);
			double averageB = (sumB/tDim);
			sliceBuff[counter++] = (int)Math.round(averageA);
			sliceBuff[counter++] = (int)Math.round(averageR);
			sliceBuff[counter++] = (int)Math.round(averageG);
			sliceBuff[counter++] = (int)Math.round(averageB);
		}
		

		return sliceBuff;
	}
	
	/**
	 * Calculates the max between 2 images
	 * @param xytBuff
	 * @return
	 */
	private int[] doIntMaxARGB(int[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = 4 * xDim * yDim;
		int[] sliceBuff = new int[4 * xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i+=4) {
			int maxA = 0;
			int maxR = 0;
			int maxG = 0;
			int maxB = 0;
			for(int t=0;t<tDim;t++) {
				int pixA = xytBuff[i + (sliceLength * t)];
				int pixR = xytBuff[i + 1 + (sliceLength * t)];
				int pixG = xytBuff[i + 2 + (sliceLength * t)];
				int pixB = xytBuff[i + 3 + (sliceLength * t)];
				if(t == 0) {
					maxA = pixA;
					maxR = pixR;
					maxG = pixG;
					maxB = pixB;
				}
				if(pixA > maxA) {
					maxA = pixA;
				}
				if(pixR > maxR) {
					maxR = pixR;
				}
				if(pixG > maxG) {
					maxG = pixG;
				}
				if(pixB > maxB) {
					maxB = pixB;
				}
				
			}

			sliceBuff[counter++] = maxA;
			sliceBuff[counter++] = maxR;
			sliceBuff[counter++] = maxG;
			sliceBuff[counter++] = maxB;
		}
		

		return sliceBuff;
	}
	
	/**
	 * Calculates the minimum between 2 images
	 * @param xytBuff
	 * @return
	 */
	private int[] doIntMinARGB(int[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = 4 * xDim * yDim;
		int[] sliceBuff = new int[4 * xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i+=4) {
			int minA = 0;
			int minR = 0;
			int minG = 0;
			int minB = 0;
			for(int t=0;t<tDim;t++) {
				int pixA = xytBuff[i + (sliceLength * t)];
				int pixR = xytBuff[i + 1 + (sliceLength * t)];
				int pixG = xytBuff[i + 2 + (sliceLength * t)];
				int pixB = xytBuff[i + 3 + (sliceLength * t)];
				if(t == 0) {
					minA = pixA;
					minR = pixR;
					minG = pixG;
					minB = pixB;
				}
				if(pixA < minA) {
					minA = pixA;
				}
				if(pixR < minR) {
					minR = pixR;
				}
				if(pixG < minG) {
					minG = pixG;
				}
				if(pixB < minB) {
					minB = pixB;
				}
				
			}

			sliceBuff[counter++] = minA;
			sliceBuff[counter++] = minR;
			sliceBuff[counter++] = minG;
			sliceBuff[counter++] = minB;
		}
		

		return sliceBuff;
	}
	
	/**
	 * Calculates the standard deviation between 2 images
	 * @param xytBuff
	 * @return
	 */
	private int[] doIntStdDevARGB(int[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = 4 * xDim * yDim;
		double[] sliceBuff = new double[4 * xDim * yDim];
		int counter = 0;
		//first do averaging
		for(int i=0;i<sliceLength;i+=4) {
			double sumA = 0;
			double sumR = 0;
			double sumG = 0;
			double sumB = 0;
			for(int t=0;t<tDim;t++) {
				int pixA = xytBuff[i + (sliceLength * t)];
				int pixR = xytBuff[i + 1 + (sliceLength * t)];
				int pixG = xytBuff[i + 2 + (sliceLength * t)];
				int pixB = xytBuff[i + 3 + (sliceLength * t)];
				sumA = (sumA + pixA);
				sumR = (sumR + pixR);
				sumG = (sumG + pixG);
				sumB = (sumB + pixB);
			}

			double averageA = (sumA/tDim);
			double averageR = (sumR/tDim);
			double averageG = (sumG/tDim);
			double averageB = (sumB/tDim);
			sliceBuff[counter++] = averageA;
			sliceBuff[counter++] = averageR;
			sliceBuff[counter++] = averageG;
			sliceBuff[counter++] = averageB;
		}
		
		
		//now calculate std dev
		counter = 0;
		int[] sliceStdDevBuff = new int[4 * xDim * yDim];
		for(int i=0;i<sliceLength;i+=4) {
			double sumA = 0;
			double sumR = 0;
			double sumG = 0;
			double sumB = 0;
			double averageA = sliceBuff[i];
			double averageR = sliceBuff[i+1];
			double averageG = sliceBuff[i+2];
			double averageB = sliceBuff[i+3];
			for(int t=0;t<tDim;t++) {
				int pixA = xytBuff[i + (sliceLength * t)];
				int pixR = xytBuff[i + 1 + (sliceLength * t)];
				int pixG = xytBuff[i + 2 + (sliceLength * t)];
				int pixB = xytBuff[i + 3 + (sliceLength * t)];
				double calcA = (pixA-averageA) * (pixA-averageA);
				double calcR = (pixR-averageR) * (pixR-averageR);
				double calcG = (pixG-averageG) * (pixG-averageG);
				double calcB = (pixB-averageB) * (pixB-averageB);
				sumA = sumA + calcA;
				sumR = sumR + calcR;
				sumG = sumG + calcG;
				sumB = sumB + calcB;
			}

			double stddevA = (Math.sqrt(sumA/tDim));
			double stddevR = (Math.sqrt(sumR/tDim));
			double stddevG = (Math.sqrt(sumG/tDim));
			double stddevB = (Math.sqrt(sumB/tDim));
			sliceStdDevBuff[counter++] = (int)Math.round(stddevA);
			sliceStdDevBuff[counter++] = (int)Math.round(stddevR);
			sliceStdDevBuff[counter++] = (int)Math.round(stddevG);
			sliceStdDevBuff[counter++] = (int)Math.round(stddevB);
			
		}
		

		return sliceStdDevBuff;
	}
	
	/**
	 * Calculates the norm between 2 images
	 * @param xytBuff
	 * @return
	 */
	private int[] doIntNormClipARGB(int[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int tDim = image.getExtents()[3];
		int sliceLength = 4 * xDim * yDim;
		int[] sliceBuff = new int[4 * xDim * yDim];
		int counter = 0;
		for(int i=0;i<sliceLength;i+=4) {
			double sumA = 0;
			double sumR = 0;
			double sumG = 0;
			double sumB = 0;
			for(int t=0;t<tDim;t++) {
				int pixA = xytBuff[i + (sliceLength * t)];
				int pixR = xytBuff[i + 1 + (sliceLength * t)];
				int pixG = xytBuff[i + 2 + (sliceLength * t)];
				int pixB = xytBuff[i + 3 + (sliceLength * t)];
				long pixSquaredA = pixA * pixA;
				long pixSquaredR = pixR * pixR;
				long pixSquaredG = pixG * pixG;
				long pixSquaredB = pixB * pixB;

				sumA = (sumA + pixSquaredA);
				sumR = (sumR + pixSquaredR);
				sumG = (sumG + pixSquaredG);
				sumB = (sumB + pixSquaredB);
			}

			
			double dA = Math.sqrt(sumA);
			double dR = Math.sqrt(sumR);
			double dG = Math.sqrt(sumG);
			double dB = Math.sqrt(sumB);
			
			if(dA > 65535) {
				dA = 65535;
			}
			if(dR > 65535) {
				dR = 65535;
			}
			if(dG > 65535) {
				dG = 65535;
			}
			if(dB > 65535) {
				dB = 65535;
			}
			
			int normA = (int)dA;
			int normR = (int)dR;
			int normG = (int)dG;
			int normB = (int)dB;
			sliceBuff[counter++] = normA;
			sliceBuff[counter++] = normR;
			sliceBuff[counter++] = normG;
			sliceBuff[counter++] = normB;
		}
		

		return sliceBuff;
	}

}
