package gov.nih.mipav.model.structures;

import java.io.IOException;

import ij.ImageStack;
import ij.process.ByteProcessor;
import ij.process.ColorProcessor;
import ij.process.FloatProcessor;
import ij.process.ImageProcessor;
import ij.process.ShortProcessor;


/**
 * @author pandyan
 * 
 * This class converts MIPAV Model Images to ImageJ Images
 *
 */
public class ModelImageToImageJConversion {

	
	
	/**
	 * converts 2D
	 * @param mi
	 * @return
	 */
	public static ImageProcessor convert2D(ModelImage mi) {
		if(!mi.is2DImage()) {
			return null;
		}
		int type = mi.getType();
		
		if(!(type == ModelStorageBase.UBYTE || type == ModelStorageBase.FLOAT || type == ModelStorageBase.USHORT || type == ModelStorageBase.ARGB)) {
			return null;
		}
		
		
		ImageProcessor ip = null;
		int width = mi.getExtents()[0];
		int height = mi.getExtents()[1];
		
		
		
		if(type == ModelStorageBase.UBYTE) {
			ip = new ByteProcessor(width,height);
			byte[] ipPixels = (byte[])ip.getPixels();
			int length = width * height;
			byte[] miPixels = new byte[length];
			
			try {
				mi.exportData(0, length, miPixels);
			}catch (IOException error) {
	            System.out.println("IO exception");
	            return null;
	        }
			
			for(int i=0;i<ipPixels.length;i++) {
				ipPixels[i] = miPixels[i];
			}
		}else if(type == ModelStorageBase.USHORT) {
			ip = new ShortProcessor(width,height);
			short[] ipPixels = (short[])ip.getPixels();
			int length = width * height;
			short[] miPixels = new short[length];
			
			try {
				mi.exportData(0, length, miPixels);
			}catch (IOException error) {
	            System.out.println("IO exception");
	            return null;
	        }
			
			for(int i=0;i<ipPixels.length;i++) {
				ipPixels[i] = miPixels[i];
			}
		}else if(type == ModelStorageBase.FLOAT) {
			ip = new FloatProcessor(width,height);
			float[] ipPixels = (float[])ip.getPixels();
			int length = width * height;
			float[] miPixels = new float[length];
			
			try {
				mi.exportData(0, length, miPixels);
			}catch (IOException error) {
	            System.out.println("IO exception");
	            return null;
	        }
			
			for(int i=0;i<ipPixels.length;i++) {
				ipPixels[i] = miPixels[i];
			}
		}else if(type == ModelStorageBase.ARGB) {
			ip = new ColorProcessor(width,height);
			int[] ipPixels = (int[])ip.getPixels();
			int length = width * height;
			byte[] miPixels = new byte[length * 4];
			
			try {
				mi.exportData(0, length*4, miPixels);
			}catch (IOException error) {
	            System.out.println("IO exception");
	            return null;
	        }
			
			
			for(int i=0,k=0;i<miPixels.length;i=i+4,k++) {
				byte b1 = miPixels[i];
				byte b2 = miPixels[i+1];
				byte b3 = miPixels[i+2];
				byte b4 = miPixels[i+3];
				
				
				int pix = (b1 << 24)
		        + ((b2 << 24) >>> 8)
		        + ((b3 << 24) >>> 16)
		        + ((b4 << 24) >>> 24);
				
				
				ipPixels[k] = pix;
				
			}
				
				
				
			
		 }
		
		
		
		
		
		
		
		return ip;
		
	}
	
	
	
	
	
	
	/**
	 * converts 3D
	 * @param mi
	 * @return
	 */
	public static ImageStack convert3D(ModelImage mi) {
		if(!mi.is3DImage()) {
			return null;
		}
		int type = mi.getType();
		
		if(!(type == ModelStorageBase.UBYTE || type == ModelStorageBase.FLOAT || type == ModelStorageBase.USHORT || type == ModelStorageBase.ARGB)) {
			return null;
		}
		ImageStack is = null;
		
		int width = mi.getExtents()[0];
		int height = mi.getExtents()[1];
		int zDim = mi.getExtents()[2];
		int sliceLength = width * height;
		
		if(type == ModelStorageBase.UBYTE) {
			is = new ImageStack(width,height);
			int start = 0;
			for(int i=0;i<zDim;i++) {
				ImageProcessor ip = new ByteProcessor(width,height);
				byte[] ipPixels = (byte[])ip.getPixels();
				byte[] miSlicePixels = new byte[sliceLength];
				
				try {
					mi.exportData(start, sliceLength, miSlicePixels);
				}catch (IOException error) {
		            System.out.println("IO exception");
		            return null;
		        }
				
				
				for(int k=0;k<ipPixels.length;k++) {
					ipPixels[k] = miSlicePixels[k];
				}
				
				is.addSlice(Integer.toString(i), ip);
				
				start = start + sliceLength;

				
			}

			
		}else if(type == ModelStorageBase.USHORT) {
			
			
			is = new ImageStack(width,height);
			int start = 0;
			for(int i=0;i<zDim;i++) {
				ImageProcessor ip = new ShortProcessor(width,height);
				short[] ipPixels = (short[])ip.getPixels();
				short[] miSlicePixels = new short[sliceLength];
				
				try {
					mi.exportData(start, sliceLength, miSlicePixels);
				}catch (IOException error) {
		            System.out.println("IO exception");
		            return null;
		        }
				
				
				for(int k=0;k<ipPixels.length;k++) {
					ipPixels[k] = miSlicePixels[k];
				}
				
				is.addSlice(Integer.toString(i), ip);
				
				start = start + sliceLength;

				
			}

		}else if(type == ModelStorageBase.FLOAT) {
			
			

			
			is = new ImageStack(width,height);
			int start = 0;
			for(int i=0;i<zDim;i++) {
				ImageProcessor ip = new FloatProcessor(width,height);
				float[] ipPixels = (float[])ip.getPixels();
				float[] miSlicePixels = new float[sliceLength];
				
				try {
					mi.exportData(start, sliceLength, miSlicePixels);
				}catch (IOException error) {
		            System.out.println("IO exception");
		            return null;
		        }
				
				
				for(int k=0;k<ipPixels.length;k++) {
					ipPixels[k] = miSlicePixels[k];
				}
				
				is.addSlice(Integer.toString(i), ip);
				
				start = start + sliceLength;

				
			}
			
			
		}else if(type == ModelStorageBase.ARGB) {
			
			is = new ImageStack(width,height);
			int start = 0;
			for(int i=0;i<zDim;i++) {
				ImageProcessor ip = new ColorProcessor(width,height);
				int[] ipPixels = (int[])ip.getPixels();
				int length = width * height * 4;
				byte[] miSlicePixels = new byte[length];
				
				try {
					mi.exportData(start, length, miSlicePixels);
				}catch (IOException error) {
		            System.out.println("IO exception");
		            return null;
		        }
				
				
				for(int j=0,k=0;j<miSlicePixels.length;j=j+4,k++) {
					byte b1 = miSlicePixels[j];
					byte b2 = miSlicePixels[j+1];
					byte b3 = miSlicePixels[j+2];
					byte b4 = miSlicePixels[j+3];
					
					
					int pix = (b1 << 24)
			        + ((b2 << 24) >>> 8)
			        + ((b3 << 24) >>> 16)
			        + ((b4 << 24) >>> 24);
					
					
					ipPixels[k] = pix;
					
				}
				
				is.addSlice(Integer.toString(i), ip);
				
				start = start + length;

				
			}
			
			
			
			
		}
		
		
		
		return is;
		
		
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
}
