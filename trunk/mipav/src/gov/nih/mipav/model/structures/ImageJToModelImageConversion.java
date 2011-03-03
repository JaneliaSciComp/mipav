package gov.nih.mipav.model.structures;

import java.io.IOException;


import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileUtility;
import ij.ImageStack;
import ij.process.ByteProcessor;
import ij.process.ColorProcessor;
import ij.process.FloatProcessor;
import ij.process.ImageProcessor;
import ij.process.ShortProcessor;



/**
 * @author pandyan
 * 
 * This class converts ImageJ Images to MIPAV Model Images
 *
 */
public class ImageJToModelImageConversion {
	
	
	/**
	 * Coverts 2D images
	 * @param ip
	 * @return
	 */
	public static ModelImage convert2D(ImageProcessor ip) {
		ModelImage mi = null;
		
		if(ip instanceof ByteProcessor) {
			byte[] ipPixels = (byte[])ip.getPixels();
			int xdim = ip.getWidth();
			int ydim = ip.getHeight();
			
			int[] extents = {xdim,ydim};
			
			mi = new ModelImage(ModelStorageBase.UBYTE,extents,"");
			
			FileInfoImageXML fileInfoXML = new FileInfoImageXML("", null, FileUtility.RAW);
    		fileInfoXML.setDataType(ModelStorageBase.UBYTE);
    		fileInfoXML.setExtents(extents);
    		fileInfoXML.setOffset(0);

    		
    		  mi.setFileInfo(fileInfoXML,0);
			
			try {
	            mi.importData(0, ipPixels, true);

	        } catch (final IOException error) {
	            System.out.println("IO exception");
	            error.printStackTrace();
	            // setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
	            return null;
	        }
			
			
		}else if(ip instanceof ShortProcessor) {
			
			short[] ipPixels = (short[])ip.getPixels();
			int xdim = ip.getWidth();
			int ydim = ip.getHeight();
			
			int[] extents = {xdim,ydim};
			
			mi = new ModelImage(ModelStorageBase.USHORT,extents,"");
			
			FileInfoImageXML fileInfoXML = new FileInfoImageXML("", null, FileUtility.RAW);
    		fileInfoXML.setDataType(ModelStorageBase.USHORT);
    		fileInfoXML.setExtents(extents);
    		fileInfoXML.setOffset(0);

    		
    		  mi.setFileInfo(fileInfoXML,0);
			
			try {
	            mi.importData(0, ipPixels, true);

	        } catch (final IOException error) {
	            System.out.println("IO exception");
	            error.printStackTrace();
	            // setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
	            return null;
	        }
			
		}else if(ip instanceof FloatProcessor) {
			
			float[] ipPixels = (float[])ip.getPixels();
			int xdim = ip.getWidth();
			int ydim = ip.getHeight();
			
			int[] extents = {xdim,ydim};
			
			mi = new ModelImage(ModelStorageBase.FLOAT,extents,"");
			
			FileInfoImageXML fileInfoXML = new FileInfoImageXML("", null, FileUtility.RAW);
    		fileInfoXML.setDataType(ModelStorageBase.FLOAT);
    		fileInfoXML.setExtents(extents);
    		fileInfoXML.setOffset(0);

    		
    		  mi.setFileInfo(fileInfoXML,0);
			
			try {
	            mi.importData(0, ipPixels, true);

	        } catch (final IOException error) {
	            System.out.println("IO exception");
	            error.printStackTrace();
	            // setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
	            return null;
	        }
			
		}else if(ip instanceof ColorProcessor) {
			int[] ipPixels = (int[])ip.getPixels();
			int xdim = ip.getWidth();
			int ydim = ip.getHeight();
			
			int[] extents = {xdim,ydim};
			byte[] miPixels = new byte[xdim*ydim*4];
			
			mi = new ModelImage(ModelStorageBase.ARGB,extents,"");
			
			FileInfoImageXML fileInfoXML = new FileInfoImageXML("", null, FileUtility.RAW);
    		fileInfoXML.setDataType(ModelStorageBase.ARGB);
    		fileInfoXML.setExtents(extents);
    		fileInfoXML.setOffset(0);
    		
    		
    		for(int i=0,k=0;i<ipPixels.length;i++,k=k+4) {
    			int ipPix = ipPixels[i];
    			byte b1 = (byte)(ipPix >>> 24);
    			byte b2 = (byte)((ipPix << 8) >>> 24);
    			byte b3 = (byte)((ipPix << 16) >>> 24);
    			byte b4 = (byte)((ipPix << 24) >>> 24);
    			miPixels[k] = b1;
    			miPixels[k+1] = b2;
    			miPixels[k+2] = b3;
    			miPixels[k+3] = b4;
    		}
    		
    		
    		try {
	            mi.importData(0, miPixels, true);

	        } catch (final IOException error) {
	            System.out.println("IO exception");
	            error.printStackTrace();
	            // setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
	            return null;
	        }
			
			
		}
		
		
		
		
		
		return mi;
		
		
	}
	
	
	/**
	 * converts 3D images
	 * @param is
	 * @return
	 */
	public static ModelImage convert3D(ImageStack is) {
		ModelImage mi = null;
		ImageProcessor ip = null;
		
		int xdim = is.getWidth();
		int ydim = is.getHeight();
		int zdim = is.getSize();
		int[] extents = {xdim,ydim,zdim};
		int sliceLength = xdim * ydim;
		int start = 0;

		for(int i=1;i<=zdim;i++) {
			ip = is.getProcessor(i);
			
			if(ip instanceof ByteProcessor) {
				byte[] ipPixels = (byte[])ip.getPixels();
				
				if(i==1) {
					mi = new ModelImage(ModelStorageBase.UBYTE,extents,"");
				}
				
				try {
		            mi.importData(start, ipPixels, true);

		        } catch (final IOException error) {
		            System.out.println("IO exception");
		            error.printStackTrace();
		            // setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		            return null;
		        }
		        
		        
				start = start + sliceLength;
				
			}else if(ip instanceof ShortProcessor) {
				
				short[] ipPixels = (short[])ip.getPixels();
				
				if(i==1) {
					mi = new ModelImage(ModelStorageBase.USHORT,extents,"");
				}
				
				try {
		            mi.importData(start, ipPixels, true);

		        } catch (final IOException error) {
		            System.out.println("IO exception");
		            error.printStackTrace();
		            // setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		            return null;
		        }
		        
		        
				start = start + sliceLength;
				
			}else if(ip instanceof FloatProcessor) {
				
				float[] ipPixels = (float[])ip.getPixels();
				
				if(i==1) {
					mi = new ModelImage(ModelStorageBase.FLOAT,extents,"");
				}
				
				try {
		            mi.importData(start, ipPixels, false);

		        } catch (final IOException error) {
		            System.out.println("IO exception");
		            error.printStackTrace();
		            // setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		            return null;
		        }
		        
		        
				start = start + sliceLength;
				
			}else if(ip instanceof ColorProcessor) {
				
				int[] ipPixels = (int[])ip.getPixels();
				byte[] miPixels = new byte[xdim*ydim*4];
				
				
				if(i==1) {
					mi = new ModelImage(ModelStorageBase.ARGB,extents,"");
				}
				
				
				for(int m=0,k=0;m<ipPixels.length;m++,k=k+4) {
	    			int ipPix = ipPixels[m];
	    			byte b1 = (byte)(ipPix >>> 24);
	    			byte b2 = (byte)((ipPix << 8) >>> 24);
	    			byte b3 = (byte)((ipPix << 16) >>> 24);
	    			byte b4 = (byte)((ipPix << 24) >>> 24);
	    			miPixels[k] = b1;
	    			miPixels[k+1] = b2;
	    			miPixels[k+2] = b3;
	    			miPixels[k+3] = b4;
	    		}
	    		
	    		
	    		try {
		            mi.importData(start, miPixels, true);

		        } catch (final IOException error) {
		            System.out.println("IO exception");
		            error.printStackTrace();
		            // setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		            return null;
		        }
				
		        start = start + sliceLength;
				
				
			}
			
			
			
	        
			
			
		}
		
		FileInfoImageXML[] fileInfoBases = new FileInfoImageXML[mi.getExtents()[2]];
		for (int k = 0; k < fileInfoBases.length; k++) {
            fileInfoBases[k] = new FileInfoImageXML("", null, FileUtility.XML);


            fileInfoBases[k].setExtents(extents);

            if(ip instanceof ByteProcessor) {
            	fileInfoBases[k].setDataType(ModelStorageBase.UBYTE);
            }else if(ip instanceof ShortProcessor) {
            	fileInfoBases[k].setDataType(ModelStorageBase.USHORT);
            }else if(ip instanceof FloatProcessor) {
            	fileInfoBases[k].setDataType(ModelStorageBase.FLOAT);
            }else if(ip instanceof ColorProcessor) {
            	fileInfoBases[k].setDataType(ModelStorageBase.ARGB);
            }
            

        }

        mi.setFileInfo(fileInfoBases);
        mi.calcMinMax();
		
		return mi;
		
		
	}

}
