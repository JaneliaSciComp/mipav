package gov.nih.mipav.model.algorithms.utilities;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;


/**
 * @author pandyan
 * algorithm for 4D Image calculation
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
	 
	 /** images **/
	 private ModelImage image, destImage;
	 
	 /** operation type **/
	 private int operationType;

	 /** whether to clip or promote **/
	 private boolean doClip;
	 
	 /** data type **/
	 private int dataType;
	
	
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
		if(operationType == MAXIMUM || operationType == MINIMUM || operationType == AVERAGE) {
			doClip = true;
		}
	}
 	
	
	
	
	
	/**
	 * run algorithm
	 */
	public void runAlgorithm() {
		
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];
		int tDim = image.getExtents()[3];
		int sliceLength = xDim * yDim;
		int volLength = sliceLength * zDim;
		byte[] byteBuff = null;
		short[] shortBuff = null;
		int[] intBuff = null;
		float[] floatBuff = null;
		byte[] byteBuffXYT = null;
		short[] shortBuffXYT = null;
		int[] intBuffXYT = null;
		float[] floatBuffXYT = null;
		dataType = image.getType();
		
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
					}else if(dataType == ModelStorageBase.SHORT || dataType == ModelStorageBase.UBYTE) {
						image.exportData(start, sliceLength, shortBuff);
						for(int k=0;k<shortBuff.length;k++) {
							shortBuffXYT[k + (sliceLength * t)] = shortBuff[k];
						}
					}else if(dataType == ModelStorageBase.INTEGER || dataType == ModelStorageBase.USHORT) {
						image.exportData(start, sliceLength, intBuff);
						for(int k=0;k<intBuff.length;k++) {
							intBuffXYT[k + (sliceLength * t)] = intBuff[k];
						}
					}else if(dataType == ModelStorageBase.FLOAT) {
						image.exportData(start, sliceLength, floatBuff);
						for(int k=0;k<floatBuff.length;k++) {
							floatBuffXYT[k + (sliceLength * t)] = floatBuff[k];
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
						}
						destImage.importData(i*sliceLength, byteSliceBuff, false);
					}else {
						short[] shortSliceBuff = null;
						if(operationType == ADD) {
							shortSliceBuff = doByteAddPromote(byteBuffXYT);
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
						}
						destImage.importData(i*sliceLength, shortSliceBuff, false);
					}else {
						int[] intSliceBuff = null;
						if(operationType == ADD) {
							intSliceBuff = doShortAddPromote(shortBuffXYT);
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
						}
						destImage.importData(i*sliceLength, intSliceBuff, false);
					}else {
						long[] longSliceBuff = null;
						if(operationType == ADD) {
							longSliceBuff = doIntAddPromote(intBuffXYT);
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
						}
						destImage.importData(i*sliceLength, floatSliceBuff, false);
					}else {
						double[] doubleSliceBuff = null;
						if(operationType == ADD) {
							doubleSliceBuff = doFloatAddPromote(floatBuffXYT);
						}
						destImage.importData(i*sliceLength, doubleSliceBuff, false);
					}
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
	
	
	
	private byte[] doByteMax(byte[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];
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
	
	
	private byte[] doByteMin(byte[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];
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
	
	
	private byte[] doByteAverage(byte[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];
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

			
			byte average = (byte)(sum/tDim);
			sliceBuff[counter] = average;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	private byte[] doByteAddClip(byte[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];
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

			sliceBuff[counter] = (byte)sum;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	
	
	
	private short[] doByteAddPromote(byte[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];
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
	
	
	
	
	
	
	private short[] doShortMax(short[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];
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
	
	
	private short[] doShortMin(short[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];
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
	
	
	private short[] doShortAverage(short[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];
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

			
			short average = (short)(sum/tDim);
			sliceBuff[counter] = average;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	private short[] doShortAddClip(short[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];
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
			}else if(dataType == ModelStorageBase.UBYTE) {
				if(sum > 255) {
					sum = 255;
				}
			}
			

			sliceBuff[counter] = (short)sum;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	
	private int[] doShortAddPromote(short[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];
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
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	private int[] doIntMax(int[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];
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
	
	
	private int[] doIntMin(int[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];
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
	
	
	private int[] doIntAverage(int[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];
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

			
			int average = (int)(sum/tDim);
			sliceBuff[counter] = average;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	private int[] doIntAddClip(int[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];
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
			}else if(dataType == ModelStorageBase.USHORT) {
				if(sum > 65535) {
					sum = 65535;
				}
			}
			
			

			sliceBuff[counter] = (int)sum;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	
	private long[] doIntAddPromote(int[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];
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
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	private float[] doFloatMax(float[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];
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
	
	
	private float[] doFloatMin(float[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];
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
	
	
	private float[] doFloatAverage(float[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];
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
	
	
	private float[] doFloatAddClip(float[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];
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
			}

			sliceBuff[counter] = (float)sum;
			counter++;
			
		}
		

		return sliceBuff;
	}
	
	
	
	
	
	private double[] doFloatAddPromote(float[] xytBuff) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];
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
	

}
