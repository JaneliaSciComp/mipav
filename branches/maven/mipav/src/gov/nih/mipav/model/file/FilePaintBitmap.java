package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;


import java.io.*;
import java.util.BitSet;



/**
 
 * 

 */
public class FilePaintBitmap  {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** File reference, it and its components are able to be modified by extending classes. */
    protected File file;

    /** File directory where the VOI is to read or written. */
    protected String fileDir;

    /** File name of the VOI. */
    protected String fileName;

    /** Image used to reference properties of the image to aid reading and writing of the VOI. */
    private final ModelImage image;

   
    /** File directory where the VOI is to read or written. */
    private int sliceNum;

    /** File name without the extension. */
    private String trimmedFileName;

    private String extension;
    
    private int length;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * VOI reader/writer constructor.
     * 
     * @param fileName file name
     * @param fileDir file directory
     * @param image image model: needed during the read process to ensure the VOI "fits" in the image space.
     * 
     * @exception IOException if there is an error making the files
     */
    public FilePaintBitmap(String fileName, final String fileDir, final ModelImage image) throws IOException {
    	int i;
        this.fileName = fileName;

        final int idx = fileName.lastIndexOf(".");

        if (idx != -1) {
            trimmedFileName = fileName.substring(0, idx);
            extension = fileName.substring(fileName.lastIndexOf("."), fileName.length());
        } else {
            trimmedFileName = fileName;
        }

        this.fileDir = fileDir;
        this.image = image;

        if (image.getNDims() >= 3) {
            sliceNum = image.getExtents()[2];
        } else {
            sliceNum = 1;
        }

        file = new File(fileDir + fileName);

        if (fileName.endsWith(".pbm")) {
            
        } else {
            fileName += ".pbm";
        }
        
        length = 1;
        int dimExtents[] = image.getExtents();

        if (dimExtents.length <= 3) {

            for (i = 0; i < dimExtents.length; i++) {
                length *= dimExtents[i];
            }
        } else {

            for (i = 0; i < 3; i++) {
                length *= dimExtents[i];
            }
        }
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    public void writePaintBitmap(BitSet paintBitmap) throws IOException {
    	RandomAccessFile raFile;
    	byte bufferByte[];
    	int i;
    	
    	try {
    	
	    	if (file.exists() == true) {
	            file.delete();
	            file = new File(fileDir + fileName);
	            raFile = new RandomAccessFile(file, "rw");
	        } else {
	            raFile = new RandomAccessFile(file, "rw");
	        }
	
	        raFile.writeBytes("MIPAV paint bitmap FILE\r\n");
	        bufferByte = new  byte[(length + 7) >> 3];
	        
	        for (i = 0; i < length; i++) {
	            if (paintBitmap.get(i)) {
	            	bufferByte[i >> 3] |= (1 << (7-(i % 8)));  
	            }
	        }
	        
	        raFile.write(bufferByte);
	        raFile.close();
    	}
    	catch (IOException error) {
    		throw error;
    	}
    	
    }
    
    public BitSet readPaintBitmap() throws IOException {
    	String paintBitmapString;
    	BitSet paintBitmap = null;
    	RandomAccessFile raFile;
    	byte bufferByte[];
    	int i;
    	
    	raFile = new RandomAccessFile(file, "r");
        paintBitmapString = raFile.readLine();

        if (paintBitmapString == null) {
            return null;
        }

        paintBitmapString = paintBitmapString.trim();
        if (paintBitmapString.length() < 23) {
        	throw (new IOException("Not a paint bitmap File MIPAV understands."));
        }
        
        paintBitmapString = paintBitmapString.substring(0, 23);

        if (paintBitmapString.equals("MIPAV paint bitmap FILE")) {
        	paintBitmap = new BitSet(length);
        	bufferByte = new  byte[(length + 7) >> 3];
        	raFile.read(bufferByte);
        	for (i = 0; i < length; i++) {

                if ((bufferByte[i >> 3] & (1 << (7-(i % 8)))) != 0) {
                    paintBitmap.set(i);
                } else {
                    paintBitmap.clear(i);
                }
            }
        	raFile.close();
        }
        else {
            throw (new IOException("Not a paint bitmap File MIPAV can read."));
        }
        
        return paintBitmap;

    }

}
