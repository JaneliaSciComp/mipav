package gov.nih.mipav.model.file;


import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBConcat;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import gov.nih.mipav.model.file.FileInfoBase.Unit;

import java.awt.*;
import java.awt.image.*;
import java.io.*;
import java.util.*;
import java.util.zip.*;

import javax.swing.JComponent;

import org.xml.sax.*;
import org.xml.sax.helpers.DefaultHandler;

import WildMagic.LibFoundation.Mathematics.*;


/**
 * File class for reading/writing Image.XML file headers and their associated images. The XML reader uses the image.xsd
 * schema for parsing. Inherits from FileXML, which parses the image.xml file based on the image.xsd
 */
public class FileImageXML extends FileXML {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** array of strings representing the tags under <image> in the xml schema. */
    private static final String[] imageStr = {"Dataset-attributes", "Subject-Information", "Scan-attributes",
            "Investigators", "Sets", "VOI", "Surface"};

    /** array of strings representing the tags under <Dataset-attributes> in the xml schema. */
    private static final String[] datasetAttributesStr = {"Description", "Linked-LUT", "Linked-image", "Image-offset",
            "Data-type", "Endianess", "Extents", "Resolutions", "Slice-spacing", "Units", "Orientation",
            "Subject-axis-orientation", "Origin", "Matrix", "Modality", "Slice-thickness", "History"};

    /** array of strings representing the tags under <Subject-information> in the xml schema. */
    private static final String[] subjectInformationStr = {"Subject-name", "Race", "Subject-ID", "Diagnosis",
            "Date-of-birth", "Height", "Weight", "Sex", "Body-part"};

    /** array of strings representing the tags under <Scan-attributes> in the xml schema. */
    private static final String[] scanAttributesStr = {"Equipment-model-name", "Scan-ID", "Protocol", "Scan-date-time"};

    /** array of strings representing the tags under <Investigators> in the xml schema. */
    private static final String[] investigatorsStr = {"Investigator-name", "Title", "Affiliation", "Email", "Phone"};

    /** array of strings representing the tags under <Sets> in the xml schema. */
    private static final String[] setStr = {"Set-description", "Parameters"};

    /** array of strings representing the tags under <Parameters> in the xml schema. */
    private static final String[] parameterStr = {"Parameter-name", "Parameter-description", "Value-type", "Value",
            "Parameter-date-time"};

    /** array of strings representing the tags under <VOI> in the xml schema. */
    private static final String[] voiStr = {"VOI-path", "Load-VOI-with-image"};

    /** array of strings representing the tags under <Surface> in the xml schema. */
    private static final String[] surfaceStr = {"Surface-path", "Load-surface-with-image", "Surface-opacity"};

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** A listing of all the additional <code>PSets</code> to be written into the saved file. */
    private Enumeration<XMLPSet> additionalSets = null;

    /** DOCUMENT ME! */
    private Vector<VOI> annotationVector = new Vector<VOI>();

    /** Index of LUT function when reading xml header. */
    private int functionIndex = -1;

    /** Vector to store the LUT functions when reading xml header. */
    private Vector<Vector2f> functionVector = new Vector<Vector2f>();

    /** Vector to hold matrices while they are being read in for the header (until they are added to the image */
    private Vector<TransMatrix> matrixVector = new Vector<TransMatrix>();

    /** Model Image associated with the file. */
    private ModelImage image;

    /** Name of the RAW (or IMG) file associated with the XML header. */
    private String imageFileName = null;

    /** Name of the file linked to this file (separate from fileName). */
    private String linkedFilename;

    /** Lookup Table associated with the file (grayscale). */
    private ModelLUT LUT;

    /** Vector to store LUT values when reading xml header. */
    private Vector<LUValue> lutVector = new Vector<LUValue>();

    /** RGB lookup table associated with the file. */
    private ModelRGB modelRGB;

    /**
     * The extension to give to the RAW file associated with the XML header. May be changed to .img if the XML header
     * links to an Analyze image (and will need to be flipped when the image is read back in).
     */
    private String rawExtension = ".raw";

    /** Thumbnail data and AWT Image. */
    private Thumbnail thumbnail = null;
    
    private String[] dataFileName;
    
    private  DTIParameters dtiparams;
    
    private int numVolumes;
    
    private float [][] gradients;
    
    private float [] bValues;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new file object.
     * 
     * @param fName File name.
     * @param fDir File directory.
     */
    public FileImageXML(String fName, String fDir) {
        super(fName, fDir);
        fileInfo = new FileInfoImageXML(fName, fDir, FileUtility.XML);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Flips image. Analyze stores its data &quot;upside down&quot;. Used if reading in an XML header attached to an
     * .img raw data file.
     * 
     * @param img Image to flip.
     * 
     * @throws IOException if there is a problem importing or exporting the image
     */
    public static final void flipTopBottom(ModelImage img) throws IOException {

        try {
            int nBuffers;
            int bufferSize;
            float[] buffer = null;
            float[] resultBuffer = null;

            if (img.getNDims() > 1) {
                bufferSize = img.getSliceSize();
            } else {
                bufferSize = img.getExtents()[0];
            }

            if (img.getNDims() == 5) {
                nBuffers = img.getExtents()[4] * img.getExtents()[3] * img.getExtents()[2];
            } else if (img.getNDims() == 4) {
                nBuffers = img.getExtents()[3] * img.getExtents()[2];
            } else if (img.getNDims() == 3) {
                nBuffers = img.getExtents()[2];
            } else {
                nBuffers = 1;
            }

            if (img.isColorImage()) {

                buffer = new float[bufferSize * 4];
                resultBuffer = new float[bufferSize * 4];
                bufferSize = bufferSize * 4;

                int i, j, k;
                int xDim = img.getExtents()[0] * 4;
                int yDim = img.getExtents()[1];

                for (k = 0; k < nBuffers; k++) {
                    img.exportData(k * bufferSize, bufferSize, buffer);

                    for (j = 0; j < yDim; j++) {

                        for (i = 0; i < xDim; i += 4) {
                            resultBuffer[ (j * xDim) + i] = 255;
                            resultBuffer[ (j * xDim) + i + 1] = buffer[ ( (yDim - 1 - j) * xDim) + i + 1];
                            resultBuffer[ (j * xDim) + i + 2] = buffer[ ( (yDim - 1 - j) * xDim) + i + 2];
                            resultBuffer[ (j * xDim) + i + 3] = buffer[ ( (yDim - 1 - j) * xDim) + i + 3];
                        }
                    }

                    img.importData(k * bufferSize, resultBuffer, false);
                }
            } else {
                buffer = new float[bufferSize];
                resultBuffer = new float[bufferSize];

                int i, j, k;
                int xDim = img.getExtents()[0];
                int yDim = img.getExtents()[1];

                for (k = 0; k < nBuffers; k++) {
                    img.exportData(k * bufferSize, bufferSize, buffer);

                    for (j = 0; j < yDim; j++) {

                        for (i = 0; i < xDim; i++) {
                            resultBuffer[ (j * xDim) + i] = buffer[ ( (yDim - 1 - j) * xDim) + i];
                        }
                    }

                    img.importData(k * bufferSize, resultBuffer, false);
                }
            }
        } catch (IOException error) {
            throw new IOException("FileXML.flipTopBottom: " + error);
        } catch (OutOfMemoryError error) {
            throw (error);
        }
    }

    /**
     * Flips image. Analyze stores its data "upside down". Used if reading in an XML header attached to an .img raw data
     * file.
     * 
     * @param buffer Buffer holding image to flip.
     * @param xmlInfo File info structure for image to flip.
     */
    public static final void flipTopBottom(float[] buffer, FileInfoImageXML xmlInfo) {
        int nBuffers;
        int bufferSize;
        float[] resultBuffer = null;

        try {

            if ( (xmlInfo.getExtents().length - 1) > 1) {
                bufferSize = xmlInfo.getExtents()[0] * xmlInfo.getExtents()[1];
            } else {
                bufferSize = xmlInfo.getExtents()[0];
            }

            if ( (xmlInfo.getExtents().length - 1) == 5) {
                nBuffers = xmlInfo.getExtents()[4] * xmlInfo.getExtents()[3] * xmlInfo.getExtents()[2];

            } else if ( (xmlInfo.getExtents().length - 1) == 4) {
                nBuffers = xmlInfo.getExtents()[3] * xmlInfo.getExtents()[2];
            } else if ( (xmlInfo.getExtents().length - 1) == 3) {
                nBuffers = xmlInfo.getExtents()[2];
            } else {
                nBuffers = 1;
            }

            resultBuffer = new float[buffer.length];

            int i, j, k;
            int xDim = xmlInfo.getExtents()[0];
            int yDim = xmlInfo.getExtents()[1];

            for (k = 0; k < nBuffers; k++) {

                for (j = 0; j < yDim; j++) {

                    for (i = 0; i < xDim; i++) {
                        resultBuffer[ (k * bufferSize) + (j * xDim) + i] = buffer[ (k * bufferSize)
                                + ( (yDim - 1 - j) * xDim) + i];
                    }
                }
            }
        } catch (OutOfMemoryError error) {
            throw (error);
        }

        System.arraycopy(resultBuffer, 0, buffer, 0, buffer.length); // buffer = resultBuffer;
    }

    /**
     * Convenience method to clear any additional sets of the enumerated list that might be written into the header
     * file.
     */
    public void clearAdditionalSets() {
        additionalSets = null;
    }

    /**
     * Prepares class for cleanup.
     */
    public void finalize() {
        image = null;
        fileInfo = null;
        LUT = null;

        if (thumbnail != null) {
            thumbnail.finalize();
        }
        additionalSets = null;
        if (annotationVector != null) {
            annotationVector.removeAllElements();
            annotationVector = null;
        }
        if (functionVector != null) {
            functionVector.removeAllElements();
            functionVector = null;
        }
        if (lutVector != null) {
            lutVector.removeAllElements();
            lutVector = null;
        }
        
        if (dataFileName != null) {
            for (int i = 0; i < dataFileName.length; i++) {
                dataFileName[i] = null;
            }
            dataFileName = null;
        }

        super.finalize();
    }

    /**
     * Returns the enumerated list of additional sets to be written into the header file.
     * 
     * @return the additional parameter sets that will be written out to the header
     */
    public Enumeration<XMLPSet> getAdditionalSets() {
        return additionalSets;
    }

    /**
     * Returns the FileInfoXML read from the file.
     * 
     * @return File info read from file, or null if it has not been read.
     */
    public FileInfoImageXML getFileInfo() {
        return (FileInfoImageXML) fileInfo;
    }

    /**
     * Gets the LUT.
     * 
     * @return ModelLUT the LUT
     */
    public ModelLUT getModelLUT() {
        return LUT;
    }

    /**
     * Gets the modelRGB.
     * 
     * @return ModelRGB the modelRGB
     */
    public ModelRGB getModelRGB() {
        return modelRGB;
    }

    /**
     * Returns the thumbnail associated with the xml file (saved in header).
     * 
     * @return Thumbnail
     */
    public Thumbnail getThumbnail() {
        return this.thumbnail;
    }
    
    /**
     * Accessor that returns the array of data file names
     * @return
     */
    public String[] getDataFileName() {
        return dataFileName;
    }

    /**
     * Reads and parses an XML header.
     * 
     * @param headerFileName file name of xml header
     * @param headerDir directory
     * @param talairach the talairach transform info to fill out if contained within the header
     * 
     * @return double array containing resolutions (for setting resolutions per FileInfoImageXML)
     * 
     * @throws IOException DOCUMENT ME!
     */
    public float[][] readHeader(String headerFileName, String headerDir, TalairachTransformInfo talairach)
            throws IOException {
        MyXMLHandler myHandler = null;
        myHandler = new MyXMLHandler((FileInfoImageXML) fileInfo, annotationVector, matrixVector, talairach);
        m_kHandler = myHandler;

        /* Pass the .xsd file to the base class for parsing: */
        if (super.readHeader(headerFileName, headerDir, "image.xsd") == null) {
            return null;
        }

        return myHandler.getResolutions();
    }

    /**
     * Reads an XML image file by reading the XML header then making a FileRaw to read the image for all filenames in
     * the file list. Only the one file directory (currently) supported.
     * 
     * @param one flag indicating one image of a 3D dataset should be read in.
     * 
     * @exception IOException if there is an error reading the file
     * @exception OutOfMemoryError if there is a problem allocating memory for the image
     * 
     * @return The image.
     * 
     * @see FileRaw
     */
    public ModelImage readImage(boolean one) throws IOException, OutOfMemoryError {
        File file;
        FileInputStream fis;
        ZipInputStream zin;
        GZIPInputStream gzin;
        CBZip2InputStream bz2in;
        int bytesRead;
        String uncompressedName = null;

        float[][] resolutions = null;

        TalairachTransformInfo talairach = new TalairachTransformInfo();
        

        fileInfo = new FileInfoImageXML(fileName, fileDir, FileUtility.XML);

        // System.err.println("Beginning header read");
        resolutions = readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory(), talairach);

        if (resolutions == null) {
            throw (new IOException("Error parsing XML Header: check debug window."));
        }

        // this will happen if the filename attribute in the xml header is not set
        if ( (imageFileName == null) || ! (new File(fileDir + File.separator + imageFileName).exists())) {
            Preferences.debug("Problem with the XML image data file name: " + fileDir + File.separator + imageFileName,
                    Preferences.DEBUG_FILEIO);

            imageFileName = FileUtility.stripExtension(fileName) + ".raw";
            fileInfo.setImageDataFileName(imageFileName);
        }

        // TODO: I don't know that this should ever happen... -- evan
        if ( (fileInfo.getFileName() == null)
                || ! (new File(fileDir + File.separator + fileInfo.getFileName()).exists())) {
            Preferences.debug("Problem with the file name stored in the XML file info: " + fileDir + File.separator
                    + fileInfo.getFileName(), Preferences.DEBUG_FILEIO);

            fileInfo.setFileName(fileName);
        }

        if (imageFileName == null) {
            imageFileName = FileUtility.stripExtension(fileName) + ".raw";
        }


        for (int i = 0; i < ((FileInfoImageXML) fileInfo).getExtents().length; i++) {
            ((FileInfoImageXML) fileInfo).setResolutions(resolutions[i][0], i);
        }

        // System.err.println("Ending header read");
        int[] extents = null;

        try {

            if (one) {
                extents = new int[ ((FileInfoImageXML) fileInfo).getExtents().length];

                for (int i = 0; i < extents.length; i++) {
                    extents[i] = ((FileInfoImageXML) fileInfo).getExtents()[i];
                }

                image = new ModelImage( ((FileInfoImageXML) fileInfo).getDataType(),
                        new int[] {extents[0], extents[1]}, fileInfo.getFileName());
            } else {
                image = new ModelImage( ((FileInfoImageXML) fileInfo).getDataType(), ((FileInfoImageXML) fileInfo)
                        .getExtents(), fileInfo.getFileName());
            }
        } catch (OutOfMemoryError error) {
            image.disposeLocal();
            throw (error);
        }

        try { // Construct a FileRaw to actually read the image.
        	 if ( !new File(fileDir + File.separator + imageFileName).exists()) { 
        		 
        		 byte[] buffer = new byte[256];
        		 if (new File(fileDir + File.separator + imageFileName + ".zip").exists()) {
        			 int offset = 0;
                     if (one) {
                         if ( ((FileInfoImageXML) fileInfo).getExtents().length > 2) {
                             offset = getOffset((FileInfoImageXML) fileInfo);
                         }
                     }
                     byte[] offsetBuff = new byte[offset];
        			 file = new File(fileDir + File.separator + imageFileName + ".zip"); 
                     try {
                         fis = new FileInputStream(file);
                     } catch (FileNotFoundException e) {
                         MipavUtil.displayError("File not found exception on fis = new FileInputStream(file) for " +
                                 (fileDir + File.separator + imageFileName  + ".zip"));
                         return null;
                     }

                     try {
                         zin = new ZipInputStream(new BufferedInputStream(fis));
                     } catch (Exception e) {
                         MipavUtil.displayError("Exception on ZipInputStream for " + fileName);
                         return null;
                     }
                    //skip past offset
                     if(offset != 0) {
	                     try {
	                         bytesRead = zin.read(offsetBuff);
	                         if(bytesRead != 256) {
                            	 buffer = getFullBuffer(zin,offsetBuff,bytesRead,offset); 
                             }
	                     } catch (IOException e) {
	                         MipavUtil.displayError("IOException on gzin.read(buffer) for " + uncompressedName);
	                         return null;
	                     }
                     }
                     int start = 0;
                     boolean endianness = fileInfo.getEndianess();
                     int type = image.getType();
                     while (true) {
                         try {
                             bytesRead = zin.read(buffer);
                             if(bytesRead == -1) {
                            	 break;
                             }
                             if(bytesRead != 256) {
                            	 buffer = getFullBuffer(zin,buffer,bytesRead,256); 
                             }

                         	if(type == ModelStorageBase.BYTE || type == ModelStorageBase.UBYTE) {
                         		image.importData(start, buffer, false);
                         		start = start + buffer.length;
                         	}else if(type == ModelStorageBase.SHORT || type == ModelStorageBase.USHORT ) {
                         		short[] shortBuff = new short[buffer.length/2];
                         		for(int i=0,k=0;i<buffer.length;i=i+2,k++) {
                         			byte[] b = {buffer[i],buffer[i+1]};
                         			shortBuff[k] = FileBase.bytesToShort(endianness, 0, b);
                         		}
                         		image.importData(start, shortBuff, false);
                         		start = start + shortBuff.length;
                         	}else if(type == ModelStorageBase.INTEGER || type == ModelStorageBase.UINTEGER) {
                         		int[] intBuff = new int[buffer.length/4];
                         		for(int i=0,k=0;i<buffer.length;i=i+4,k++) {
                         			byte[] b = {buffer[i],buffer[i+1],buffer[i+2],buffer[i+3]};
                         			intBuff[k] = FileBase.bytesToInt(endianness, 0, b);
                         		}
                         		image.importData(start, intBuff, false);
                         		start = start + intBuff.length;
                         	}else if(type == ModelStorageBase.FLOAT) {
                         		float[] floatBuff = new float[buffer.length/4];
                         		for(int i=0,k=0;i<buffer.length;i=i+4,k++) {
                         			byte[] b = {buffer[i],buffer[i+1],buffer[i+2],buffer[i+3]};
                         			floatBuff[k] = FileBase.bytesToFloat(endianness, 0, b);
                         		}
                         		image.importData(start, floatBuff, false);
                         		start = start + floatBuff.length;                             
                         		                              
                         	}else if(type == ModelStorageBase.ARGB) {
                         		image.importData(start, buffer, false);
                         		start = start + buffer.length;
                         	}else if(type == ModelStorageBase.ARGB_USHORT) {
                         		short[] shortBuff = new short[buffer.length/2];
                         		for(int i=0,k=0;i<buffer.length;i=i+2,k++) {
                         			byte[] b = {buffer[i],buffer[i+1]};
                         			shortBuff[k] = FileBase.bytesToShort(endianness, 0, b);
                         		}
                         		image.importData(start, shortBuff, false);
                         		start = start + shortBuff.length;
                         	}else if(type == ModelStorageBase.ARGB_FLOAT) {
                         		float[] floatBuff = new float[buffer.length/4];
                         		for(int i=0,k=0;i<buffer.length;i=i+4,k++) {
                         			byte[] b = {buffer[i],buffer[i+1],buffer[i+2],buffer[i+3]};
                         			floatBuff[k] = FileBase.bytesToFloat(endianness, 0, b);
                         		}
                         		image.importData(start, floatBuff, false);
                         		start = start + floatBuff.length;
                         	}
                         } catch (IOException e) {
                             MipavUtil.displayError("IOException on gzin.read(buffer) for " + uncompressedName);
                             return null;
                         } 
                     }
                     //close the compressed stream if open
                     if(zin != null) {
                     	zin.close();
                     }   
        		 } else if (new File(fileDir + File.separator + imageFileName + ".gz").exists()) {
        			 int offset = 0;
                     if (one) {
                         if ( ((FileInfoImageXML) fileInfo).getExtents().length > 2) {
                             offset = getOffset((FileInfoImageXML) fileInfo);
                         }
                     }
                     byte[] offsetBuff = new byte[offset];
        			 file = new File(fileDir + File.separator + imageFileName + ".gz"); 
                     try {
                         fis = new FileInputStream(file);
                     } catch (FileNotFoundException e) {
                         MipavUtil.displayError("File not found exception on fis = new FileInputStream(file) for " +
                                 (fileDir + File.separator + imageFileName  + ".gz"));
                         return null;
                     }
                     try {
                         gzin = new GZIPInputStream(new BufferedInputStream(fis));
                     } catch (IOException e) {
                         MipavUtil.displayError("IOException on GZIPInputStream for " + fileName);
                         return null;
                     }
                     //skip past offset
                     if(offset != 0) {
	                     try {
	                         bytesRead = gzin.read(offsetBuff);
	                         if(bytesRead != 256) {
                            	 buffer = getFullBuffer(gzin,offsetBuff,bytesRead,offset); 
                             }
	                     } catch (IOException e) {
	                         MipavUtil.displayError("IOException on gzin.read(buffer) for " + uncompressedName);
	                         return null;
	                     }
                     }
                     int start = 0;
                     boolean endianness = fileInfo.getEndianess();
                     int type = image.getType();
                     while (true) {
                         try {
                             bytesRead = gzin.read(buffer);
                             if(bytesRead == -1) {
                            	 break;
                             }
                             if(bytesRead != 256) {
                            	 buffer = getFullBuffer(gzin,buffer,bytesRead,256); 
                             }

                         	if(type == ModelStorageBase.BYTE || type == ModelStorageBase.UBYTE) {
                         		image.importData(start, buffer, false);
                         		start = start + buffer.length;
                         	}else if(type == ModelStorageBase.SHORT || type == ModelStorageBase.USHORT ) {
                         		short[] shortBuff = new short[buffer.length/2];
                         		for(int i=0,k=0;i<buffer.length;i=i+2,k++) {
                         			byte[] b = {buffer[i],buffer[i+1]};
                         			shortBuff[k] = FileBase.bytesToShort(endianness, 0, b);
                         		}
                         		image.importData(start, shortBuff, false);
                         		start = start + shortBuff.length;
                         	}else if(type == ModelStorageBase.INTEGER || type == ModelStorageBase.UINTEGER) {
                         		int[] intBuff = new int[buffer.length/4];
                         		for(int i=0,k=0;i<buffer.length;i=i+4,k++) {
                         			byte[] b = {buffer[i],buffer[i+1],buffer[i+2],buffer[i+3]};
                         			intBuff[k] = FileBase.bytesToInt(endianness, 0, b);
                         		}
                         		image.importData(start, intBuff, false);
                         		start = start + intBuff.length;
                         	}else if(type == ModelStorageBase.FLOAT) {
                         		float[] floatBuff = new float[buffer.length/4];
                         		for(int i=0,k=0;i<buffer.length;i=i+4,k++) {
                         			byte[] b = {buffer[i],buffer[i+1],buffer[i+2],buffer[i+3]};
                         			floatBuff[k] = FileBase.bytesToFloat(endianness, 0, b);
                         		}
                         		image.importData(start, floatBuff, false);
                         		start = start + floatBuff.length;                             
                         		                              
                         	}else if(type == ModelStorageBase.ARGB) {
                         		image.importData(start, buffer, false);
                         		start = start + buffer.length;
                         	}else if(type == ModelStorageBase.ARGB_USHORT) {
                         		short[] shortBuff = new short[buffer.length/2];
                         		for(int i=0,k=0;i<buffer.length;i=i+2,k++) {
                         			byte[] b = {buffer[i],buffer[i+1]};
                         			shortBuff[k] = FileBase.bytesToShort(endianness, 0, b);
                         		}
                         		image.importData(start, shortBuff, false);
                         		start = start + shortBuff.length;
                         	}else if(type == ModelStorageBase.ARGB_FLOAT) {
                         		float[] floatBuff = new float[buffer.length/4];
                         		for(int i=0,k=0;i<buffer.length;i=i+4,k++) {
                         			byte[] b = {buffer[i],buffer[i+1],buffer[i+2],buffer[i+3]};
                         			floatBuff[k] = FileBase.bytesToFloat(endianness, 0, b);
                         		}
                         		image.importData(start, floatBuff, false);
                         		start = start + floatBuff.length;
                         	}
                         } catch (IOException e) {
                             MipavUtil.displayError("IOException on gzin.read(buffer) for " + uncompressedName);
                             return null;
                         } 
                     }
                     //close the compressed stream if open
                     if(gzin != null) {
                     	gzin.close();
                     }
        		 }else if (new File(fileDir + File.separator + imageFileName + ".bz2").exists()) {
        			 int offset = 0;
                     if (one) {
                         if ( ((FileInfoImageXML) fileInfo).getExtents().length > 2) {
                             offset = getOffset((FileInfoImageXML) fileInfo);
                         }
                     }
                     byte[] offsetBuff = new byte[offset];
        			 file = new File(fileDir + File.separator + imageFileName + ".bz2"); 
                     try {
                         fis = new FileInputStream(file);
                     } catch (FileNotFoundException e) {
                         MipavUtil.displayError("File not found exception on fis = new FileInputStream(file) for " + 
                                 (fileDir + File.separator + imageFileName  + ".bz2"));
                         return null;
                     }
                     
                     try {
                     fis.read();
                     }
                     catch (IOException e) {
                         MipavUtil.displayError("IOException on fis.read() trying to read byte B");
                         return null;
                     }
                     
                     try {
                         fis.read();
                         }
                         catch (IOException e) {
                             MipavUtil.displayError("IOException on fis.read() trying to read byte Z");
                             return null;
                         }

                     try {
                         bz2in = new CBZip2InputStream(new BufferedInputStream(fis));
                     } catch (Exception e) {
                         MipavUtil.displayError("Exception on CBZip2InputStream for " + fileName);
                         return null;
                     }
                   //skip past offset
                     if(offset != 0) {
	                     try {
	                         bytesRead = bz2in.read(offsetBuff);
	                         if(bytesRead != 256) {
                            	 buffer = getFullBuffer(bz2in,offsetBuff,bytesRead,offset); 
                             }
	                     } catch (IOException e) {
	                         MipavUtil.displayError("IOException on gzin.read(buffer) for " + uncompressedName);
	                         return null;
	                     }
                     }
                     int start = 0;
                     boolean endianness = fileInfo.getEndianess();
                     int type = image.getType();
                     while (true) {
                         try {
                             bytesRead = bz2in.read(buffer);
                             if(bytesRead == -1) {
                            	 break;
                             }
                             if(bytesRead != 256) {
                            	 buffer = getFullBuffer(bz2in,buffer,bytesRead,256); 
                             }

                         	if(type == ModelStorageBase.BYTE || type == ModelStorageBase.UBYTE) {
                         		image.importData(start, buffer, false);
                         		start = start + buffer.length;
                         	}else if(type == ModelStorageBase.SHORT || type == ModelStorageBase.USHORT ) {
                         		short[] shortBuff = new short[buffer.length/2];
                         		for(int i=0,k=0;i<buffer.length;i=i+2,k++) {
                         			byte[] b = {buffer[i],buffer[i+1]};
                         			shortBuff[k] = FileBase.bytesToShort(endianness, 0, b);
                         		}
                         		image.importData(start, shortBuff, false);
                         		start = start + shortBuff.length;
                         	}else if(type == ModelStorageBase.INTEGER || type == ModelStorageBase.UINTEGER) {
                         		int[] intBuff = new int[buffer.length/4];
                         		for(int i=0,k=0;i<buffer.length;i=i+4,k++) {
                         			byte[] b = {buffer[i],buffer[i+1],buffer[i+2],buffer[i+3]};
                         			intBuff[k] = FileBase.bytesToInt(endianness, 0, b);
                         		}
                         		image.importData(start, intBuff, false);
                         		start = start + intBuff.length;
                         	}else if(type == ModelStorageBase.FLOAT) {
                         		float[] floatBuff = new float[buffer.length/4];
                         		for(int i=0,k=0;i<buffer.length;i=i+4,k++) {
                         			byte[] b = {buffer[i],buffer[i+1],buffer[i+2],buffer[i+3]};
                         			floatBuff[k] = FileBase.bytesToFloat(endianness, 0, b);
                         		}
                         		image.importData(start, floatBuff, false);
                         		start = start + floatBuff.length;                             
                         		                              
                         	}else if(type == ModelStorageBase.ARGB) {
                         		image.importData(start, buffer, false);
                         		start = start + buffer.length;
                         	}else if(type == ModelStorageBase.ARGB_USHORT) {
                         		short[] shortBuff = new short[buffer.length/2];
                         		for(int i=0,k=0;i<buffer.length;i=i+2,k++) {
                         			byte[] b = {buffer[i],buffer[i+1]};
                         			shortBuff[k] = FileBase.bytesToShort(endianness, 0, b);
                         		}
                         		image.importData(start, shortBuff, false);
                         		start = start + shortBuff.length;
                         	}else if(type == ModelStorageBase.ARGB_FLOAT) {
                         		float[] floatBuff = new float[buffer.length/4];
                         		for(int i=0,k=0;i<buffer.length;i=i+4,k++) {
                         			byte[] b = {buffer[i],buffer[i+1],buffer[i+2],buffer[i+3]};
                         			floatBuff[k] = FileBase.bytesToFloat(endianness, 0, b);
                         		}
                         		image.importData(start, floatBuff, false);
                         		start = start + floatBuff.length;
                         	}
                         } catch (IOException e) {
                             MipavUtil.displayError("IOException on gzin.read(buffer) for " + uncompressedName);
                             return null;
                         } 
                     }
                     //close the compressed stream if open
                     if(bz2in != null) {
                     	bz2in.close();
                     }
        		 }
        		 
        	 }else {
        		 FileRaw rawFile;
                 String readFileName = new String(imageFileName);

                 // imageFileName was parsed from the "image".xml file.
                 rawFile = new FileRaw(fileDir + File.separator + readFileName, (FileInfoImageXML) fileInfo, FileBase.READ);

                 long offset = 0L;

                 if (one) {

                     if ( ((FileInfoImageXML) fileInfo).getExtents().length > 2) {
                         offset = (long)getOffset((FileInfoImageXML) fileInfo);
                     }
                 }

                 linkProgress(rawFile);

                 fireProgressStateChanged(0);

                 rawFile.readImage(image, offset);

        	 }
        	 
        	
            

            if (one) {
                ((FileInfoImageXML) fileInfo).setExtents(extents);
            }

            FileInfoImageXML[] nFileInfos;

            int nDims = ((FileInfoImageXML) fileInfo).getExtents().length;

            if (nDims > 2) { // Set file info

                int length = ((FileInfoImageXML) fileInfo).getExtents()[2];

                if (nDims > 3) {
                    length *= ((FileInfoImageXML) fileInfo).getExtents()[3];

                    if (nDims > 4) {
                        length *= ((FileInfoImageXML) fileInfo).getExtents()[4];
                    }
                }

                nFileInfos = new FileInfoImageXML[length];

                boolean separateResolutions = false;

                // System.err.println("Length: " + length + " res length: " + (resolutions[0].length));

                if (length == resolutions[0].length) {
                    separateResolutions = true;
                }

                // System.err.println("Sep res: " + separateResolutions);
                for (int i = 0; i < length; i++) {
                    nFileInfos[i] = (FileInfoImageXML) (fileInfo.clone());

                    if (separateResolutions) {

                        for (int index = 0; index < nDims; index++) {
                            nFileInfos[i].setResolutions(resolutions[index][i], index);
                        }
                    }
                }

                image.setFileInfo(nFileInfos);
                updateOriginInfo(nFileInfos);
            } else {
                ((FileInfoImageXML) fileInfo).setResolutions(resolutions[0][0], 0);
                ((FileInfoImageXML) fileInfo).setResolutions(resolutions[1][0], 1);
                image.setFileInfo((FileInfoImageXML) fileInfo, 0);
            }
            
            if (uncompressedName != null) {
                // Delete the input uncompressed file
                File uncompressedFile;
                uncompressedFile = new File(uncompressedName);
                try {
                    uncompressedFile.delete();
                } catch (SecurityException sc) {
                    MipavUtil.displayError("Security error occurs while trying to delete " +
                                           uncompressedName);
                }    
            }

        } catch (IOException error) {

            error.printStackTrace();
            image.disposeLocal();
            throw new IOException("FileXML: " + error);
        } catch (OutOfMemoryError e) {
            image.disposeLocal();

            //
            throw (e);
        }

        image.getMatrixHolder().replaceMatrices(matrixVector);
        // image.setMatrix(((FileInfoImageXML) fileInfo).getMatrix());

        // if talairach data was populated, add it
        if (talairach.getOrigOrient() != null) {
            image.setTalairachTransformInfo(talairach);
        }

        if ( (annotationVector != null) && (annotationVector.size() > 0)) {
            VOI currentVOI = null;

            for (int i = 0; i < annotationVector.size(); i++) {
                currentVOI = null;
                currentVOI = annotationVector.elementAt(i);
                image.registerVOI(currentVOI);
            }

            annotationVector.removeAllElements();
        }
        
        if (dtiparams != null){
            if (gradients!=null){
                dtiparams.setGradients(gradients);
            }
            image.setDTIParameters(dtiparams);  
            
        }
        
        
        
        

        return image;
    }
    
    
    
    private byte[] getFullBuffer(InputStream in, byte[] buff, int off, int fullBufferSize) {
    	
    	int bytesRead = 0;
    	int offset = off;
    	while(offset != fullBufferSize || bytesRead != -1)
    	
    		
    	 try {
    		 
             bytesRead = in.read(buff,offset,fullBufferSize-offset);

             if(bytesRead == -1) {
            	 break;
             }
             if(bytesRead == 0) {
            	 break;
             }
             offset = offset + bytesRead;

             if(offset == fullBufferSize) {
            	 break;
             }
           
         } catch (IOException e) {
             MipavUtil.displayError("IOException on gzin.read(buffer)");
             
         }
    	
         if(offset != 256) {
        	 //this means that we reached the end of file...so lets return the appropriate sized buffer
        	 byte[] buffShortened = new byte[offset];
        	 for(int i=0;i<offset;i++) {
        		 buffShortened[i] = buff[i];
        	 }
        	 return buffShortened;
         }else {
        	 return buff;
         }
    	
    	
    }

    /**
     * Reads an XML image file by reading the header then making a FileRaw to read the file. Image data is left in
     * buffer. If the fileInfo cannot be found, the header will be located and read first.
     * 
     * @param buffer Image buffer to store image data into. It is equal to the header length.
     * 
     * @throws IOException if there is an error reading the file
     * @throws OutOfMemoryError if there was a problem allocating enough memory
     * 
     * @see FileRaw
     */
    public void readImage(float[] buffer) throws IOException, OutOfMemoryError {
        String tempDir;
        File file;
        String uncompressedName = null;
        FileInputStream fis;
        ZipInputStream zin;
        FileOutputStream out;
        int bytesRead;
        GZIPInputStream gzin;
        CBZip2InputStream bz2in;

        TalairachTransformInfo talairach = new TalairachTransformInfo();

        float[][] resolutions = null;

        if (fileInfo == null) { // if no file info yet, make it.
            fileInfo = new FileInfoImageXML(fileName, fileDir, FileUtility.XML);

            resolutions = readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory(), talairach);

            if (resolutions == null) {
                throw (new IOException("Cannot read image because of analyze header file error"));
            }
        }

        try { // Construct a FileRaw to actually read the image.

            FileRaw rawFile;

            if (imageFileName == null) {
                imageFileName = FileUtility.stripExtension(fileName) + ".raw";
            }
             
            fileDir = fileInfo.getFileDirectory();
            if ( !new File(fileDir + File.separator + imageFileName).exists()) {
                tempDir = Preferences.getFileTempDir();
                if (tempDir == null) {
                    tempDir = System.getProperty("user.home") + File.separator + "mipav" + File.separator + "tempDir" + File.separator;
                }
                else {
                    tempDir = tempDir + File.separator;
                }
                file = new File(tempDir);
                if (!file.exists()) {
                    file.mkdirs();
                }
                uncompressedName = tempDir + imageFileName;
                if (new File(fileDir + File.separator + imageFileName + ".zip").exists()) {
                    file = new File(fileDir + File.separator + imageFileName + ".zip"); 
                    int totalBytesRead = 0;

                    try {
                        fis = new FileInputStream(file);
                    } catch (FileNotFoundException e) {
                        MipavUtil.displayError("File not found exception on fis = new FileInputStream(file) for " +
                                (fileDir + File.separator + imageFileName  + ".zip"));
                        return;
                    }

                    try {
                        zin = new ZipInputStream(new BufferedInputStream(fis));
                    } catch (Exception e) {
                        MipavUtil.displayError("Exception on ZipInputStream for " + fileName);
                        return;
                    }
                    fileDir = tempDir;
                    try {
                        out = new FileOutputStream(uncompressedName);
                    } catch (IOException e) {
                        MipavUtil.displayError("IOException on FileOutputStream for " + uncompressedName);
                        return;
                    }
                    byte[] buf = new byte[256];
                    
                    try {
                        while (zin.getNextEntry() != null) {
                            while (true) {
                                
                                bytesRead = zin.read(buf);
                                
                                if (bytesRead == -1) {
                                    break;
                                }
                
                                totalBytesRead += bytesRead;
                                    out.write(buf, 0, bytesRead);
                                
                            }
                        } // while (zin.getNextEntry() != null)
                    }
                    catch (IOException e) {
                        MipavUtil.displayError("IOException in loop reading entries");
                        return;
                    }

                    try {
                        out.close();
                    } catch (IOException e) {
                        MipavUtil.displayError("IOException on out.close for " + uncompressedName);
                        return;
                    }
                }
                else if (new File(fileDir + File.separator + imageFileName + ".gz").exists()) {
                    file = new File(fileDir + File.separator + imageFileName + ".gz"); 
                    int totalBytesRead = 0;

                    try {
                        fis = new FileInputStream(file);
                    } catch (FileNotFoundException e) {
                        MipavUtil.displayError("File not found exception on fis = new FileInputStream(file) for " +
                                (fileDir + File.separator + imageFileName  + ".gz"));
                        return;
                    }

                    try {
                        gzin = new GZIPInputStream(new BufferedInputStream(fis));
                    } catch (IOException e) {
                        MipavUtil.displayError("IOException on GZIPInputStream for " + fileName);
                        return;
                    }
                    fileDir = tempDir;
                    try {
                        out = new FileOutputStream(uncompressedName);
                    } catch (IOException e) {
                        MipavUtil.displayError("IOException on FileOutputStream for " + uncompressedName);
                        return;
                    }
                    byte[] buf = new byte[256];

                    while (true) {
                        try {
                            bytesRead = gzin.read(buf);
                        } catch (IOException e) {
                            MipavUtil.displayError("IOException on gzin.read(buf) for " + uncompressedName);
                            return;
                        }

                        if (bytesRead == -1) {
                            break;
                        }

                        totalBytesRead += bytesRead;
                        try {
                            out.write(buf, 0, bytesRead);
                        } catch (IOException e) {
                            MipavUtil.displayError("IOException on out.write for " + uncompressedName);
                            return;
                        }
                    }

                    try {
                        out.close();
                    } catch (IOException e) {
                        MipavUtil.displayError("IOException on out.close for " + uncompressedName);
                        return;
                    }
                }
                else if (new File(fileDir + File.separator + imageFileName + ".bz2").exists()) {
                    int totalBytesRead = 0;
                    file = new File(fileDir + File.separator + imageFileName + ".bz2"); 
                    try {
                        fis = new FileInputStream(file);
                    } catch (FileNotFoundException e) {
                        MipavUtil.displayError("File not found exception on fis = new FileInputStream(file) for " + 
                                (fileDir + File.separator + imageFileName  + ".bz2"));
                        return;
                    }
                    
                    try {
                    fis.read();
                    }
                    catch (IOException e) {
                        MipavUtil.displayError("IOException on fis.read() trying to read byte B");
                        return;
                    }
                    
                    try {
                        fis.read();
                        }
                        catch (IOException e) {
                            MipavUtil.displayError("IOException on fis.read() trying to read byte Z");
                            return;
                        }

                    try {
                        bz2in = new CBZip2InputStream(new BufferedInputStream(fis));
                    } catch (Exception e) {
                        MipavUtil.displayError("Exception on CBZip2InputStream for " + fileName);
                        return;
                    }
                    fileDir = tempDir;
                    try {
                        out = new FileOutputStream(uncompressedName);
                    } catch (IOException e) {
                        MipavUtil.displayError("IOException on FileOutputStream for " + uncompressedName);
                        return;
                    }
                    byte[] buf = new byte[256];

                    while (true) {
                        try {
                            bytesRead = bz2in.read(buf);
                        } catch (IOException e) {
                            MipavUtil.displayError("IOException on bz2in.read(buf) for " + uncompressedName);
                            return;
                        }

                        if (bytesRead == -1) {
                            break;
                        }

                        totalBytesRead += bytesRead;
                        try {
                            out.write(buf, 0, bytesRead);
                        } catch (IOException e) {
                            MipavUtil.displayError("IOException on out.write for " + uncompressedName);
                            return;
                        }
                    }

                    try {
                        out.close();
                    } catch (IOException e) {
                        MipavUtil.displayError("IOException on out.close for " + uncompressedName);
                        return;
                    }     
                }
                else {
                    MipavUtil.displayWarning("Raw file not found: " + imageFileName + ".  Aborting XML readImage(float[] buffer)!");
            
                    return;
                }
            }

            rawFile = new FileRaw(imageFileName, fileDir, (FileInfoImageXML) fileInfo,
                    FileBase.READ);
            rawFile.readImage(buffer, 0L, ((FileInfoImageXML) fileInfo).getDataType());

            if (fileInfo.getFileName().indexOf(".img") == (fileInfo.getFileName().length() - 4)) {
                flipTopBottom(buffer, (FileInfoImageXML) fileInfo);
            }

            rawFile.finalize();
            
            if (uncompressedName != null) {
                // Delete the input uncompressed file
                File uncompressedFile;
                uncompressedFile = new File(uncompressedName);
                try {
                    uncompressedFile.delete();
                } catch (SecurityException sc) {
                    MipavUtil.displayError("Security error occurs while trying to delete " +
                                           uncompressedName);
                }    
            }
        } catch (IOException error) {
            Preferences.debug("IOException in FileImageXML:readImage(float [])", Preferences.DEBUG_FILEIO);
            throw new IOException("FileXML: " + error);
        } catch (OutOfMemoryError e) {
            throw (e);
        }

        return;
    }

    /**
     * Method to replace the enumerated list of additional sets to be written into the header file. Any existing value
     * is lost.
     * 
     * @param moreSets additional parameter sets to be written
     */
    public void setAdditionalSets(Enumeration<XMLPSet> moreSets) {
        additionalSets = moreSets;
    }

    /**
     * Accessor to set the file name (used when reading XML multiFile).
     * 
     * @param fName file name of image to read.
     */
    public void setFileName(String fName) {
        fileName = fName;
    }

    /**
     * Sets the model LUT.
     * 
     * @param lut ModelLUT the LUT
     */
    public void setModelLUT(ModelLUT lut) {
        this.LUT = lut;
    }

    /**
     * Sets the model RGB.
     * 
     * @param modelRGB ModelRGB the modelRGB
     */
    public void setModelRGB(ModelRGB modelRGB) {

        // System.err.println("in setModelRGB() of FileXML");
        this.modelRGB = modelRGB;
    }

    /**
     * Changes the extension of the image data file associated with this XML header.
     * 
     * @param ext the new file extension (such as .img)
     */
    public void setRawExtension(String ext) {
        rawExtension = ext;
    }

    /**
     * Sets the thumbnail data (array of shorts).
     * 
     * @param xDim the buffer length in the x dimension
     * @param yDim the buffer length in the y dimension
     * @param data the thumbnail data
     */
    public void setThumbnailData(int xDim, int yDim, int[] data) {
        this.thumbnail = new Thumbnail(xDim, yDim, data);
        // System.err.println("Created thumbnail");
    }

    /**
     * Writes the XML header information out to the given filename and path.
     * 
     * @param img image associated with header
     * @param options the options to use when writing out the file
     * @param headerName file name to write to
     * @param headerDir name of directory to write to
     * @param changeDims if true indicates that the image is changing dimensionality (e.g., 3D to 2D)
     * 
     * @return if header write was successful
     * 
     * @throws IOException if a file I/O problem is encoutered while writing the header
     */
    @SuppressWarnings("unchecked")
    public boolean writeHeader(ModelImage img, FileWriteOptions options, String headerName, String headerDir,
            boolean changeDims) throws IOException {
        boolean simple = false; // A simple write only writes absolutely neccessary information
        String temp;
        int nDims;
        int i;
        int[] extents;
        int[] units;
        int[] axis;
        float[] resolutions;
        float sliceSpacing;
        float sliceThickness;
        float[] origin;
        FileInfoBase myFileInfo;
        FileWriter fw;
        File headerFile;

        TalairachTransformInfo talairach = img.getTalairachTransformInfo();

        int startSlice = 0;
        int endSlice = 0;

        int endTime = 0;

        if (options != null) {
            startSlice = options.getBeginSlice();
            endSlice = options.getEndSlice();
            endTime = options.getEndTime();
        } else {

            if (img.getNDims() > 3) {
                endSlice = img.getExtents()[2];
                endTime = img.getExtents()[3];
            } else if (img.getNDims() == 3) {
                endSlice = img.getExtents()[2];
            }
        }

        // System.err.println("Change dims is: " + changeDims);

        nDims = img.getNDims();

        if (changeDims) {
            nDims--;

            // one last check to see if image is going from 4D to 2D
            if ( (img.getNDims() == 4) && (img.getFileInfo()[0].getExtents()[2] == 1)
                    && (img.getFileInfo()[0].getExtents()[3] == 1)) {
                nDims--;
            }
        }

        myFileInfo = img.getFileInfo(0); // A safeguard in case the file is not XML

        try {
            fileInfo = (FileInfoImageXML) img.getFileInfo(0);
        } catch (ClassCastException e) { // If it isn't, catch the exception

            // and make a new fileInfo
            fileInfo = new FileInfoImageXML(headerName, headerDir, FileUtility.XML);

            if (img.getFileInfo(0) instanceof FileInfoMincHDF) {
                ((FileInfoMincHDF) img.getFileInfo(0)).convertPatientInfo((FileInfoImageXML) fileInfo);

                // System.err.println("finfo subjectid: " + ((FileInfoImageXML)fileInfo).getSubjectID());
            }

            // if we are writing a non-XML file but are doing the SRB header-only writing
            // we want to write non-simple (include extra info that has been set)
            if (options.writeHeaderOnly()) {
                simple = false;
            } else {
                simple = true; // Write the header without all the Analyze info
            }
        }

        // set up the NDAR specific information for header writing
        if (options.writeHeaderOnly()) {
        	
        	
            NDARWriteData data = options.getNDARData();
            if(data != null) {
            	((FileInfoImageXML) fileInfo).setSubjectID(data.validGUID);	
            	((FileInfoImageXML) fileInfo).setHistory(data.zipFileName);
            }
            
            
            //add psets and parameters
            HashMap<String,XMLPSet> pSetsHashMap = (HashMap<String, XMLPSet>) options.getPSetsHashMap();
            if(pSetsHashMap != null) {
            	Set<String> psetKeys = pSetsHashMap.keySet();
            	Iterator<String> psetIter = psetKeys.iterator();
            	while(psetIter.hasNext()) {
            		String psetDesc = psetIter.next();
            		XMLPSet pset = pSetsHashMap.get(psetDesc);
            		((FileInfoImageXML) fileInfo).addPset(psetDesc, pset);
            		Hashtable<String, XMLParameter> parameterTable = pset.getTable();
            		Set<String> paramKeys = parameterTable.keySet();
            		Iterator<String> paramIter = paramKeys.iterator();
            		while(paramIter.hasNext()) {
            			String paramName = paramIter.next();
            			XMLParameter param = parameterTable.get(paramName);
            			String value =  param.getValue();
            			String description = param.getDescription();
            			String type = param.getValueType();
            			((FileInfoImageXML) fileInfo).getPSet(psetDesc).addParameter(paramName);
            			((FileInfoImageXML) fileInfo).getPSet(psetDesc).getParameter(paramName).setValue(value);
            			((FileInfoImageXML) fileInfo).getPSet(psetDesc).getParameter(paramName).setValueType(type);
            			((FileInfoImageXML) fileInfo).getPSet(psetDesc).getParameter(paramName).setDescription(description);
            		}
            	}
            }
            
        }

        fileName = headerName + rawExtension;

        extents = img.getFileInfo()[0].getExtents();

        headerFile = new File(headerDir + headerName + ".xml");
        fw = new FileWriter(headerFile);
        bw = new BufferedWriter(fw);

        bw.write(XML_HEADER);
        bw.newLine();
        bw.write(MIPAV_HEADER);
        bw.newLine();

        if (fileName.equals(headerName + ".raw")) {

            // the xml image reader assumes that if filename is not specified, it is the same as the header file with
            // the .raw extension
            openTag("image xmlns:xsi=\"" + W3C_XML_SCHEMA + "-instance\" nDimensions=\"" + nDims + "\"", true);
        } else {

            // we want to connect the header to a non-default file (e.g., an xml pointing to an analyze .img file)
            openTag("image xmlns:xsi=\"" + W3C_XML_SCHEMA + "-instance\" filename=\"" + fileName + "\" nDimensions=\""
                    + nDims + "\"", true);
        }

        openTag(imageStr[0], true);

        if ( !simple) {
            temp = fileInfo.getImageDescription();

            if ( (temp != null) && !temp.equalsIgnoreCase("")) {
                closedTag(datasetAttributesStr[0], temp);
            }
        }

        if ( !simple) {
            temp = ((FileInfoImageXML) fileInfo).getHistory();

            if ( (temp != null) && !temp.equalsIgnoreCase("")) {
                closedTag(datasetAttributesStr[16], temp);
            }
        }

        if ( !simple) {

            if (linkedFilename == null) {
                temp = ((FileInfoImageXML) fileInfo).getLinkedImagePath();

                if ( (temp != null) && !temp.equalsIgnoreCase("")) {
                    closedTag(datasetAttributesStr[2], temp);
                }
            }
        }

        // for save image set only
        if (linkedFilename != null) {
            closedTag(datasetAttributesStr[2], linkedFilename);
        }

        // retain any image offset if saving from XML to XML, but disregard if from non-XML to XML, since the offset was
        // to a different image file type
        if (myFileInfo.getFileFormat() == FileUtility.XML || myFileInfo.getFileFormat() == FileUtility.XML_MULTIFILE) {
            closedTag(datasetAttributesStr[3], new Integer(myFileInfo.getOffset()).toString());
        } else {
            closedTag(datasetAttributesStr[3], "0");
        }

        closedTag(datasetAttributesStr[4], ModelStorageBase.getBufferTypeStr(img.getType()));

        if (myFileInfo.getEndianess()) {
            closedTag(datasetAttributesStr[5], "Big");
        } else {
            closedTag(datasetAttributesStr[5], "Little");
        }

        for (i = 0; i < nDims; i++) {
            closedTag(datasetAttributesStr[6], new Integer(extents[i]).toString());
        }

        int numRes = 1;

        if (nDims == 3) {
            numRes = img.getFileInfo()[0].getExtents()[2];
        } else if (nDims > 3) {
            numRes = img.getFileInfo()[0].getExtents()[2] * img.getFileInfo()[0].getExtents()[3];
        }

        // compare all resolutions and determine if separate resolutions should
        // be saved, or if one resolution (if all same) should be written in the header
        float[] firstRes = img.getFileInfo()[0].getResolutions();
        float[] compRes = null;
        boolean separateRes = false;
        int j;

        for (i = 0; (i < numRes) && !separateRes; i++) {
            compRes = img.getFileInfo()[i].getResolutions();

            for (j = 0; j < nDims; j++) {

                if (firstRes[j] != compRes[j]) {

                    // System.err.println("different resolutions found: " + firstRes[j] + " and " + compRes[j]);
                    separateRes = true;

                    break;
                }
            }
        }
        int start = startSlice;
        // if all resolutions are the same, only write 1
        if ( !separateRes) {
            numRes = 1;
            start = 0;
        }

        if (separateRes) {

            if (nDims == 3) {
                numRes = endSlice + 1;
            } else {
                numRes = (endSlice * endTime) + 1;
            }
        }

        for (int resIndex = start; resIndex < numRes; resIndex++) {
            resolutions = img.getFileInfo()[resIndex].getResolutions();
            openTag("Resolutions", true);

            for (i = 0; i < nDims; i++) {
                closedTag("Resolution", new Float(resolutions[i]).toString());
            }

            openTag("Resolutions", false);
        }

        // only write out slice spacing if nDims > 2
        // if slice spacing is 0, set it to be the z resolution
        // otherwise keep the value (only applicable to XML and DICOM)
        if (nDims > 2) {
            sliceSpacing = myFileInfo.getResolution(2);

            closedTag(datasetAttributesStr[8], String.valueOf(sliceSpacing));
        }

        if (nDims > 2) {
            sliceThickness = myFileInfo.getSliceThickness();

            closedTag(datasetAttributesStr[15], String.valueOf(sliceThickness));
        }

        units = myFileInfo.getUnitsOfMeasure();

        for (i = 0; i < nDims; i++) {
            closedTag(datasetAttributesStr[9], (Unit.getUnitFromLegacyNum(units[i])).toString());
        }

        
            
        int compression = myFileInfo.getCompressionType();
        if (compression == FileInfoBase.COMPRESSION_ZIP) {
            closedTag("Compression", "zipped");
        }
        else if (compression == FileInfoBase.COMPRESSION_GZIP) {
            closedTag("Compression", "gzipped");
        }
        else if (compression == FileInfoBase.COMPRESSION_BZIP2) {
            closedTag("Compression", "bz2zipped");
        }
        else {
            closedTag("Compression", "none");    
        }

        int orient = myFileInfo.getImageOrientation();

        closedTag(datasetAttributesStr[10], FileInfoBase.getImageOrientationStr(orient));

        axis = myFileInfo.getAxisOrientation();

        for (i = 0; (i < nDims) && (i < 3); i++) {
            temp = FileInfoBase.getAxisOrientationStr(axis[i]);

            if ( (temp != null) && !temp.equalsIgnoreCase("")) {
                closedTag(datasetAttributesStr[11], temp);
            }
        }

        origin = myFileInfo.getOrigin();

        for (i = 0; (i < nDims) && (i < origin.length); i++) {
            temp = new Float(origin[i]).toString();
                
            if ( (temp != null) && !temp.equalsIgnoreCase("")) {
                closedTag(datasetAttributesStr[12], temp);
            }
        }

        // matrix...
        // if ((!changeDims || nDims == 4) && ((image.getMatrix().getNCols() == (nDims+1)) ||
        // ((image.getMatrix().getNCols() == 4) &&
        // (nDims == 4)))) {

        // BEN: change this here to save all associated matrices...
        LinkedHashMap<String,TransMatrix> matrixMap = img.getMatrixHolder().getMatrixMap();
        Iterator<String> iter = matrixMap.keySet().iterator();

        // boolean to see if talairach transform info should be used
        @SuppressWarnings("unused")
        boolean useTal = false;

        String currentKey = null;

        boolean useMatrices = true;
        while (useMatrices && iter.hasNext()) {
            currentKey = iter.next();

            TransMatrix tMatrix = matrixMap.get(currentKey);
            if (tMatrix != null && tMatrix.getDim() >= img.getNDims()) {
                openTag(datasetAttributesStr[13], true);

                // if the dimensions arent correct for image/matrix, switch it to correct dim and identity
                if (tMatrix.getDim() != Math.min( (nDims + 1), 4)) {
                    tMatrix = new TransMatrix(Math.min(nDims + 1, 4), TransMatrix.TRANSFORM_ANOTHER_DATASET);
                }

                closedTag("Transform-ID", TransMatrix.getTransformIDStr(tMatrix.getTransformID()));
                if (tMatrix.getTransformID() == TransMatrix.TRANSFORM_TALAIRACH_TOURNOUX) {
                    useTal = true;
                }

                // check to see if it is sagittal or coronal with an identity transform matrix (convert)
                if (tMatrix.isIdentity()) {

                    if (orient == FileInfoBase.SAGITTAL) {
                        // same for 2D or 3D, 2D doesn't use last row/col
                        tMatrix.set(0, 1, 0, 0, 0, 0, -1, 0, -1, 0, 0, 0, 0, 0, 0, 1);

                    } else if (orient == FileInfoBase.CORONAL) {
                        // same for 2D or 3D, 2D doesn't use last row/col
                        tMatrix.set(1, 0, 0, 0, 0, 0, -1, 0, 0, 1, 0, 0, 0, 0, 0, 1);

                    }
                }

                for (i = 0; i < tMatrix.getDim(); i++) {

                    for (j = 0; j < tMatrix.getDim(); j++) {
                        closedTag("Data", new Double(tMatrix.get(i, j)).toString());
                    }
                }

                openTag(datasetAttributesStr[13], false);
            }

        }

        closedTag(datasetAttributesStr[14], FileInfoBase.getModalityStr(myFileInfo.getModality()));

        // If model RGB is null and the LUT is not null, only write out the LUT values and
        // LUT functions. Otherwise, write out the model RGB and ignore the LUT
        if ( (modelRGB == null) && (LUT != null)) {

            // System.err.println("Writing LUT in writeHeader()");
            int nPts = 0;
            float[] x = null;
            float[] y = null;
            TransferFunction function = null;

            // transfer
            openTag("LUT-functions", true);
            closedTag("Function-type", "transfer");
            function = LUT.getTransferFunction();
            nPts = function.size();
            x = new float[nPts];
            y = new float[nPts];
            function.exportArrays(x, y);

            for (int ind = 0; ind < nPts; ind++) {
                closedTag("Point", Float.toString(x[ind]) + "," + Float.toString(y[ind]));
            }

            openTag("LUT-functions", false);

            // alpha
            openTag("LUT-functions", true);
            closedTag("Function-type", "alpha");
            function = LUT.getAlphaFunction();
            nPts = function.size();
            x = new float[nPts];
            y = new float[nPts];
            function.exportArrays(x, y);

            for (int ind = 0; ind < nPts; ind++) {
                closedTag("Point", Float.toString(x[ind]) + "," + Float.toString(y[ind]));
            }

            openTag("LUT-functions", false);

            // red
            openTag("LUT-functions", true);
            closedTag("Function-type", "red");
            function = LUT.getRedFunction();
            nPts = function.size();
            x = new float[nPts];
            y = new float[nPts];
            function.exportArrays(x, y);

            for (int ind = 0; ind < nPts; ind++) {
                closedTag("Point", Float.toString(x[ind]) + "," + Float.toString(y[ind]));
            }

            openTag("LUT-functions", false);

            // green
            openTag("LUT-functions", true);
            closedTag("Function-type", "green");
            function = LUT.getGreenFunction();
            nPts = function.size();
            x = new float[nPts];
            y = new float[nPts];
            function.exportArrays(x, y);

            for (int ind = 0; ind < nPts; ind++) {
                closedTag("Point", Float.toString(x[ind]) + "," + Float.toString(y[ind]));
            }

            openTag("LUT-functions", false);

            // blue
            openTag("LUT-functions", true);
            closedTag("Function-type", "blue");
            function = LUT.getBlueFunction();
            nPts = function.size();
            x = new float[nPts];
            y = new float[nPts];
            function.exportArrays(x, y);

            for (int ind = 0; ind < nPts; ind++) {
                closedTag("Point", Float.toString(x[ind]) + "," + Float.toString(y[ind]));
            }

            openTag("LUT-functions", false);

            // Now get LUT itself
            int height;

            height = LUT.getExtents()[1];

            openTag("LUT", true);

            for (int k = 0; k < height; k++) {
                closedTag("LUValue", Float.toString(LUT.getFloat(0, k)) + "," + Float.toString(LUT.getFloat(1, k))
                        + "," + Float.toString(LUT.getFloat(2, k)) + "," + Float.toString(LUT.getFloat(3, k)));
            }

            openTag("LUT", false);
        } // endif LUT !=null
        else if (modelRGB != null) {

            // System.err.println("Writing modelRGB in FileXML.writeHeader()");
            int nPts = 0;
            float[] x = null;
            float[] y = null;
            TransferFunction function = null;

            // red
            openTag("LUT-functions", true);
            closedTag("Function-type", "red");
            function = modelRGB.getRedFunction();
            nPts = function.size();
            x = new float[nPts];
            y = new float[nPts];
            function.exportArrays(x, y);

            for (int ind = 0; ind < nPts; ind++) {
                closedTag("Point", Float.toString(x[ind]) + "," + Float.toString(y[ind]));
            }

            openTag("LUT-functions", false);

            // green
            openTag("LUT-functions", true);
            closedTag("Function-type", "green");
            function = modelRGB.getGreenFunction();
            nPts = function.size();
            x = new float[nPts];
            y = new float[nPts];
            function.exportArrays(x, y);

            for (int ind = 0; ind < nPts; ind++) {
                closedTag("Point", Float.toString(x[ind]) + "," + Float.toString(y[ind]));
            }

            openTag("LUT-functions", false);

            // blue
            openTag("LUT-functions", true);
            closedTag("Function-type", "blue");
            function = modelRGB.getBlueFunction();
            nPts = function.size();
            x = new float[nPts];
            y = new float[nPts];
            function.exportArrays(x, y);

            for (int ind = 0; ind < nPts; ind++) {
                closedTag("Point", Float.toString(x[ind]) + "," + Float.toString(y[ind]));
            }

            openTag("LUT-functions", false);

            // Now get LUT itself
            int height;

            height = modelRGB.getExtents()[1];

            openTag("LUT", true);

            for (int k = 0; k < height; k++) {
                closedTag("LUValue", Float.toString(modelRGB.getFloat(0, k)) + ","
                        + Float.toString(modelRGB.getFloat(1, k)) + "," + Float.toString(modelRGB.getFloat(2, k)) + ","
                        + Float.toString(modelRGB.getFloat(3, k)));
            }

            openTag("LUT", false);

        }

        openTag(imageStr[0], false);

        // if this is being saved from an XML file, save off XML specific information if included
        // otherwise ignore these fields as they do not exist in the file info
        if ( !simple || img.getFileInfo()[0] instanceof FileInfoMincHDF) {

            // System.err.println("looking for subject id");

            // IFF there is a subject ID, we will include this tag
            if ( ( ((FileInfoImageXML) fileInfo).getSubjectID() != null)
                    && ! ((FileInfoImageXML) fileInfo).getSubjectID().equalsIgnoreCase("")) {
                openTag(imageStr[1], true);

                // System.err.println("subjectID found");

                temp = ((FileInfoImageXML) fileInfo).getSubjectName();

                if ( (temp != null) && !temp.equalsIgnoreCase("")) {
                    closedTag(subjectInformationStr[0], temp);
                }

                temp = ((FileInfoImageXML) fileInfo).getRace();

                if ( (temp != null) && !temp.equalsIgnoreCase("")) {
                    closedTag(subjectInformationStr[1], temp);
                }

                closedTag(subjectInformationStr[2], ((FileInfoImageXML) fileInfo).getSubjectID());

                temp = ((FileInfoImageXML) fileInfo).getDiagnosis();

                if ( (temp != null) && !temp.equalsIgnoreCase("")) {
                    closedTag(subjectInformationStr[3], temp);
                }

                temp = ((FileInfoImageXML) fileInfo).getDOB();

                if ( (temp != null) && !temp.equalsIgnoreCase("")) {
                    closedTag(subjectInformationStr[4], temp);
                }

                int hw = ((FileInfoImageXML) fileInfo).getHeight();

                if (hw > 0) {
                    closedTag(subjectInformationStr[5], new Integer(hw).toString());
                }

                hw = ((FileInfoImageXML) fileInfo).getWeight();

                if (hw > 0) {
                    closedTag(subjectInformationStr[6], new Integer(hw).toString());
                }

                temp = ((FileInfoImageXML) fileInfo).getSex();

                if ( (temp != null) && !temp.equalsIgnoreCase("")) {
                    closedTag(subjectInformationStr[7], temp);
                }

                temp = ((FileInfoImageXML) fileInfo).getBodyPart();

                if ( (temp != null) && !temp.equalsIgnoreCase("")) {
                    closedTag(subjectInformationStr[8], temp);
                }

                openTag(imageStr[1], false);
            } // end if contains subject ID

            if ( ( ((FileInfoImageXML) fileInfo).getScanDate() != null)
                    && ( ((FileInfoImageXML) fileInfo).getScanTime() != null)) {
                openTag(imageStr[2], true);

                temp = ((FileInfoImageXML) fileInfo).getEquipmentName();

                if ( (temp != null) && !temp.equalsIgnoreCase("")) {
                    closedTag(scanAttributesStr[0], temp);
                }

                temp = ((FileInfoImageXML) fileInfo).getScanID();

                if ( (temp != null) && !temp.equalsIgnoreCase("")) {
                    closedTag(scanAttributesStr[1], temp);
                }

                temp = ((FileInfoImageXML) fileInfo).getProtocol();

                if ( (temp != null) && !temp.equalsIgnoreCase("")) {
                    closedTag(scanAttributesStr[2], temp);
                }

                temp = ((FileInfoImageXML) fileInfo).getScanDate() + "T" + ((FileInfoImageXML) fileInfo).getScanTime();
                closedTag(scanAttributesStr[3], temp);

                openTag(imageStr[2], false);
            } // end if contains scan attributes

            boolean[] invest = ((FileInfoImageXML) fileInfo).getInvestigatorsComplete();

            for (i = 0; i < 3; i++) {

                if (invest[i]) {

                    openTag(imageStr[3], true);
                    closedTag(investigatorsStr[0], ((FileInfoImageXML) fileInfo).getInvestigator(i).getName());

                    temp = ((FileInfoImageXML) fileInfo).getInvestigator(i).getTitle();

                    if ( (temp != null) && !temp.equalsIgnoreCase("")) {
                        closedTag(investigatorsStr[1], temp);
                    }

                    temp = ((FileInfoImageXML) fileInfo).getInvestigator(i).getAffiliation();

                    if ( (temp != null) && !temp.equalsIgnoreCase("")) {
                        closedTag(investigatorsStr[2], temp);
                    }

                    temp = ((FileInfoImageXML) fileInfo).getInvestigator(i).getEmail();

                    if ( (temp != null) && !temp.equalsIgnoreCase("")) {
                        closedTag(investigatorsStr[3], temp);
                    }

                    temp = ((FileInfoImageXML) fileInfo).getInvestigator(i).getPhone();

                    if ( (temp != null) && !temp.equalsIgnoreCase("")) {
                        closedTag(investigatorsStr[4], temp);
                    }

                    openTag(imageStr[3], false);
                }

            }
        }

        /** Save off the thumbnail data if data is compressed */
        if (compression != FileInfoBase.COMPRESSION_NONE) {
            ModelImage tempImage = img;
            int colorFactor = 1;

            if (img.isColorImage()) {
                colorFactor = 4;
            }

            if (img.getNDims() > 2) {

                // get the middle slice (2D)
                float[] data = new float[img.getSliceSize() * colorFactor];

                img.exportSliceXY(img.getExtents()[2] / 2, data);
                tempImage = new ModelImage(img.getType(), new int[] {img.getExtents()[0], img.getExtents()[1]},
                        "thumbnail");
                tempImage.importData(0, data, true);
                // temp.getFileInfo()[0].setResolutions(image.getFileInfo()[0].getResolutions()[0], 0);
                // temp.getFileInfo()[0].setResolutions(image.getFileInfo()[0].getResolutions()[1], 1); new
                // ViewJFrameImage(temp, null, new Dimension(0, 0), image.getUserInterface());
            }

            // the max resolution for thumbnail image is 64x64... so determine the largest extent (x or y),
            // set that to 64 and find the opposing extent
            int xDim = 64;
            int yDim = 64;

            if (tempImage.getExtents()[0] > tempImage.getExtents()[1]) {
                yDim = Math.round( ((float) tempImage.getExtents()[1] / (float) tempImage.getExtents()[0]) * 64f);
            } else if (tempImage.getExtents()[0] < tempImage.getExtents()[1]) {
                xDim = Math.round( ((float) tempImage.getExtents()[0] / (float) tempImage.getExtents()[1]) * 64f);
            }

            // System.err.println("XDim: " + xDim + " YDim: " + yDim);

            openTag("Thumbnail xDim=\"" + xDim + "\" yDim=\"" + yDim + "\"", true);

            // first create a transformed smaller image
            float[] res = new float[] {tempImage.getFileInfo()[0].getResolutions()[0],
                    tempImage.getFileInfo()[0].getResolutions()[1]};

            double factorX = (double) tempImage.getExtents()[0] / (double) xDim;
            double factorY = (double) tempImage.getExtents()[1] / (double) yDim;

            if ( (factorX % 1) == 0) {
                factorX -= .02;
            }

            if ( (factorY % 1) == 0) {
                factorY -= .02;
            }

            res[0] *= factorX;
            res[1] *= factorY;

            // System.err.println("ResX: " + res[0] + " ResY: " + res[1] + " factorX: " + factorX + " factorY: " +
            // factorY);

            TransMatrix transMat = new TransMatrix(3);

            // System.err.println(transMat);

            AlgorithmTransform algoTrans = null;

            algoTrans = new AlgorithmTransform(tempImage, transMat, AlgorithmTransform.BSPLINE3, res[0], res[1], xDim,
                    yDim, false, false, false);
            algoTrans.run();

            ModelImage thumbnailImage = algoTrans.getTransformedImage();

            thumbnailImage.calcMinMax();
            algoTrans.finalize();

            // if the image is not color, convert it to color
            if ( !thumbnailImage.isColorImage()) {
                ModelImage tempConcat = new ModelImage(ModelStorageBase.ARGB, thumbnailImage.getExtents(),
                        "thumbnail_rgb");
                AlgorithmRGBConcat algoRGB = new AlgorithmRGBConcat(thumbnailImage, thumbnailImage, thumbnailImage,
                        tempConcat, true, true, 255.0f, true);

                algoRGB.run();
                thumbnailImage.disposeLocal();
                thumbnailImage = tempConcat;
                thumbnailImage.calcMinMax();
                tempConcat = null;
            }

            // new ViewJFrameImage(thumbnailImage, null, new Dimension(0, 0), image.getUserInterface());

            // export the thumbnail image into an int[] buffer and to write out the XML tags
            int[] thumbnailData = new int[xDim * yDim * 4];

            thumbnailImage.exportData(0, thumbnailData.length, thumbnailData);
            thumbnailImage.disposeLocal();
            thumbnailImage = null;

            int numIterations = (int) Math.ceil( ( ((double) thumbnailData.length / 4.0) * 3.0) / 18.0);

            // System.err.println("Number of lines: " + numIterations);
            for (int idx = 0, thumbIndex = 0; idx < numIterations; idx++, thumbIndex += 24) {
                closedTag("Thumbnail-data", getFormattedThumbnailLine(thumbnailData, thumbIndex));
            }

            openTag("Thumbnail", false);

        }

        // only save off PSets (additional xml space to store anything) if this was originally an xml file
        if ( !simple) {
            writeSet(bw, ((FileInfoImageXML) fileInfo).getPSetHashtable().elements());

            Enumeration<String> voiEnum = ((FileInfoImageXML) fileInfo).getVOIKeys();

            while (voiEnum.hasMoreElements()) {
                openTag(imageStr[5], true);
                temp = voiEnum.nextElement();
                closedTag(voiStr[0], temp);
                closedTag(voiStr[1], Boolean.toString( ((FileInfoImageXML) fileInfo).getVOI(temp).getDisplay()));
                openTag(imageStr[5], false);
            }

            Enumeration<String> surfaceEnum = ((FileInfoImageXML) fileInfo).getSurfaceKeys();

            while (surfaceEnum.hasMoreElements()) {
                openTag(imageStr[6], true);
                temp = surfaceEnum.nextElement();
                closedTag(surfaceStr[0], temp);
                closedTag(surfaceStr[1], Boolean.toString( ((FileInfoImageXML) fileInfo).getSurface(temp).getDisplay()));
                closedTag(surfaceStr[2], Float.toString( ((FileInfoImageXML) fileInfo).getSurface(temp).getOpacity()));
                openTag(imageStr[6], false);
            }

        }

        // we can write in other elements if we need, say for conversion from
        // one type of FileInfoBase to this one.
        if (additionalSets != null) {
            writeSet(bw, additionalSets);
        }

        // if the Talairach Transform Info is not null, write it
        // into the XML header
        if (talairach != null) {

            openTag("Talairach", true);
            closedTag("origAC", Float.toString(talairach.getOrigAC().X));
            closedTag("origAC", Float.toString(talairach.getOrigAC().Y));
            closedTag("origAC", Float.toString(talairach.getOrigAC().Z));

            closedTag("origPC", Float.toString(talairach.getOrigPC().X));
            closedTag("origPC", Float.toString(talairach.getOrigPC().Y));
            closedTag("origPC", Float.toString(talairach.getOrigPC().Z));

            for (i = 0; i < 3; i++) {
                closedTag("origDim", Integer.toString(talairach.getOrigDim()[i]));
            }
            
            for (i = 0; i < 3; i++) {
                closedTag("origOrigin", Float.toString(talairach.getOrigOrigin()[i]));
            }

            for (i = 0; i < 3; i++) {
                closedTag("origRes", Float.toString(talairach.getOrigRes()[i]));
            }

            for (i = 0; i < 3; i++) {

                for (j = 0; j < 3; j++) {
                    closedTag("origOrient", Float.toString(talairach.getOrigOrient()[i][j]));
                }
            }

            closedTag("acpcPC", Float.toString(talairach.getAcpcPC().X));
            closedTag("acpcPC", Float.toString(talairach.getAcpcPC().Y));
            closedTag("acpcPC", Float.toString(talairach.getAcpcPC().Z));

            closedTag("acpcRes", Float.toString(talairach.getAcpcRes()));

            if (talairach.isTlrc()) {
                openTag("tlrcInfo", true);

                closedTag("acpcMin", Float.toString(talairach.getAcpcMin().X));
                closedTag("acpcMin", Float.toString(talairach.getAcpcMin().Y));
                closedTag("acpcMin", Float.toString(talairach.getAcpcMin().Z));

                closedTag("acpcMax", Float.toString(talairach.getAcpcMax().X));
                closedTag("acpcMax", Float.toString(talairach.getAcpcMax().Y));
                closedTag("acpcMax", Float.toString(talairach.getAcpcMax().Z));

                for (i = 0; i < 7; i++) {
                    closedTag("tlrcRes", Float.toString(talairach.getTlrcRes()[i]));
                }

                openTag("tlrcInfo", false);
            }

            openTag("Talairach", false);
        }
        
            dtiparams = img.getDTIParameters();
            if (dtiparams != null) {
                openTag("DTIParameters", true);
                    closedTag("volumeNum", Integer.toString(dtiparams.getNumVolumes()));
                
                if (dtiparams.getbValues() != null){
                    //Write bvalues to XML header on a single line
                    String bVals = "";
                    for (i=0; i<dtiparams.getNumVolumes(); i++){
                        bVals = bVals + Float.toString(dtiparams.getbValues()[i])+"/";
                    }
                    closedTag("bValues", bVals);
                }
                if (dtiparams.getGradients() != null){
                  //Write gradients to XML header on a single line per volume
                    openTag("VolumeGradients", true);
                    
                    for (i=0; i<dtiparams.getNumVolumes(); i++){
                    
                        closedTag("gradient", Float.toString(dtiparams.getGradients()[i][0]) + "," + Float.toString(dtiparams.getGradients()[i][1]) + "," + Float.toString(dtiparams.getGradients()[i][2]) );
                    }
                    openTag("VolumeGradients", false);
                }
                openTag("DTIParameters", false);
            }

        openTag("image", false);

        bw.close();

        return true;
    }

    /**
     * Writes an XML image with the given options.
     * 
     * @param img Model image to be written to disk
     * @param options tells how and where to save file
     * 
     * @throws IOException if there is a problem writing to the file
     */
    public void writeImage(ModelImage img, FileWriteOptions options) throws IOException {
        String fhName;
        int index;

        FileInfoBase infoClone = (FileInfoBase) img.getFileInfo(0).clone();

        int num = img.getExtents().length;

        setModelRGB(options.getRGBTa());

        // create new extents to match up with specified beginning/end slices/times
        int[] newExtents = null;

        if (num == 4) {
            newExtents = new int[4];
            newExtents[3] = (options.getEndTime() - options.getBeginTime() + 1);
            newExtents[2] = (options.getEndSlice() - options.getBeginSlice() + 1);
            newExtents[1] = img.getExtents()[1];
            newExtents[0] = img.getExtents()[0];
        } else if (num == 3) {
            newExtents = new int[3];
            newExtents[2] = (options.getEndSlice() - options.getBeginSlice() + 1);
            newExtents[1] = img.getExtents()[1];
            newExtents[0] = img.getExtents()[0];
        } else {
            newExtents = new int[2];
            newExtents[1] = img.getExtents()[1];
            newExtents[0] = img.getExtents()[0];
        }

        // sets the extents to the fileinfo (which will be replaced after)
        img.getFileInfo()[0].setExtents(newExtents);

        index = fileName.lastIndexOf(".");

        if (index != -1) {
            fhName = fileName.substring(0, index);
        } else {
            fhName = fileName.substring(0);
        }

        if (options.isSaveAs()) {
            this.fileName = fhName + rawExtension;
        }

        if (options.getXMLLinkedFilename() != null) {
            this.linkedFilename = options.getXMLLinkedFilename();
        }

        if ( !fileName.endsWith(rawExtension)) {
            fileName = fhName + rawExtension;
        }

        // if the image is to be saved as multiple files, call the correct file and header-saving functions
        if (options.isMultiFile()) {

            FileRaw rawFile;

            rawFile = new FileRaw(img.getFileInfo(0));
            linkProgress(rawFile);

            if (img.getNDims() == 3) {
            	if ( !options.writeHeaderOnly()) {
            		rawFile.writeImage3DTo2D(img, options, rawExtension);
            	}
                writeHeader3DTo2D(img, fhName, fileDir, options);
            } else if (img.getNDims() == 4) {
            	if ( !options.writeHeaderOnly()) {
            		rawFile.writeImage4DTo3D(img, options, rawExtension);
            	}
                writeHeader4DTo3D(img, fhName, fileDir, options);
            }

            dataFileName = rawFile.getDataFileName().clone();
            rawFile.finalize();
            img.setFileInfo(infoClone, 0);
        } else {

            try {

                if ( !options.writeHeaderOnly()) {
                    FileRaw rawFile;

                    rawFile = new FileRaw(fileName, fileDir, img.getFileInfo(0), FileBase.READ_WRITE);

                    linkProgress(rawFile);

                    // options.setFileName(rawName);
                    rawFile.writeImage(img, options);
                    rawFile.close();
                    rawFile.finalize();
                }

                // System.err.println("wrote image");
                // System.err.println("file info extents length: " + image.getFileInfo()[0].getExtents().length);
                // System.err.println("image extents length: " + image.getExtents().length);
                if ( (img.getFileInfo()[0].getExtents().length == 3) && (img.getFileInfo()[0].getExtents()[2] == 1)) {

                    // System.out.println( "Converting 3d to 2d because 3rd extent is 1" );
                    writeHeader(img, options, fhName, fileDir, true);
                } else if ( (img.getFileInfo()[0].getExtents().length == 4)
                        && (img.getFileInfo()[0].getExtents()[3] == 1)) {

                    // System.out.println( "Converting 4d to 3d because 4th extent is 1" );
                    writeHeader(img, options, fhName, fileDir, true);
                } else {
                    writeHeader(img, options, fhName, fileDir, false);
                }

                img.setFileInfo(infoClone, 0);
            } catch (IOException error) {
                img.setFileInfo(infoClone, 0);
                throw new IOException("FileXMLWrite: " + error);
            } catch (OutOfMemoryError error) {
                img.setFileInfo(infoClone, 0);
                throw (error);
            }
        }
    }

    /**
     * Helper method to calculate the offset for getting only the middle analyze image slice from the 3D file.
     * 
     * @param xmlInfo File info.
     * 
     * @return offset
     */
    private static int getOffset(FileInfoImageXML xmlInfo) {
        int offset = xmlInfo.getExtents()[0] * xmlInfo.getExtents()[1] * (xmlInfo.getExtents()[2] / 2);

        switch (xmlInfo.getDataType()) {

            case ModelStorageBase.BOOLEAN:
            case ModelStorageBase.BYTE:
            case ModelStorageBase.UBYTE:
                break;

            case ModelStorageBase.SHORT:
            case ModelStorageBase.USHORT:
                offset *= 2;
                break;

            case ModelStorageBase.FLOAT:
            case ModelStorageBase.INTEGER:
            case ModelStorageBase.UINTEGER:
                offset *= 4;
                break;

            case ModelStorageBase.LONG:
            case ModelStorageBase.DOUBLE:
                offset *= 8;
                break;

            case ModelStorageBase.ARGB:
                offset *= 3;
                break;

            case ModelStorageBase.ARGB_USHORT:
                offset *= 6;
                break;
        }

        return offset;
    }

    /**
     * Adds the LUT functions collected within the functionVector.
     */
    private void addFunctionToLUT() {

        if (LUT == null) {
            LUT = new ModelLUT(ModelLUT.GRAY, 256, new int[] {4, 256});
        }

        int n;
        float[] x;
        float[] y;
        float[] z;

        x = new float[functionVector.size()];
        y = new float[functionVector.size()];
        z = new float[functionVector.size()];

        Preferences.debug("Function Points:" + "\n", Preferences.DEBUG_FILEIO);

        for (n = 0; n < functionVector.size(); n++) {
            x[n] = functionVector.elementAt(n).X;
            y[n] = functionVector.elementAt(n).Y;
            z[n] = 0;

            Preferences.debug("x: " + x[n] + ", y: " + y[n] + "\n", Preferences.DEBUG_FILEIO);
        }

        // System.out.println("Function index is: " + functionIndex);
        switch (functionIndex) {

            case 0:

                // System.out.println(functionVector);
                LUT.getTransferFunction().importArrays(x, y, functionVector.size());
                break;

            case 1:
                LUT.getAlphaFunction().importArrays(x, y, functionVector.size());
                break;

            case 2:
                LUT.getRedFunction().importArrays(x, y, functionVector.size());
                break;

            case 3:
                LUT.getGreenFunction().importArrays(x, y, functionVector.size());
                break;

            case 4:
                LUT.getBlueFunction().importArrays(x, y, functionVector.size());
        }
    }

    /**
     * Adds the ModelRGB functions contained within the functionVector.
     */
    private void addFunctionToRGB() {

        if (modelRGB == null) {
            modelRGB = new ModelRGB(new int[] {4, 256});
        }

        int n;
        float[] x;
        float[] y;
        float[] z;

        x = new float[functionVector.size()];
        y = new float[functionVector.size()];
        z = new float[functionVector.size()];

        Preferences.debug("Function Points:" + "\n", Preferences.DEBUG_FILEIO);

        for (n = 0; n < functionVector.size(); n++) {
            x[n] = functionVector.elementAt(n).X;
            y[n] = functionVector.elementAt(n).Y;
            z[n] = 0;

            Preferences.debug("x: " + x[n] + ", y: " + y[n] + "\n", Preferences.DEBUG_FILEIO);
        }

        // System.out.println("Function index is: " + functionIndex);
        switch (functionIndex) {

            case 2:
                modelRGB.getRedFunction().importArrays(x, y, functionVector.size());
                break;

            case 3:
                modelRGB.getGreenFunction().importArrays(x, y, functionVector.size());
                break;

            case 4:
                modelRGB.getBlueFunction().importArrays(x, y, functionVector.size());
                break;

            default:
                // System.err.println("Got function index of: " + functionIndex);
                // do nothing
        }
    }

    /**
     * Adds LUValue for either LUT or modelRGB.
     * 
     * @param s String
     */
    private void addLUValue(String s) {
        float red = 0, green = 0, blue = 0;
        StringTokenizer st = new StringTokenizer(s, ",");

        try {
            red = Float.parseFloat(st.nextToken());
            green = Float.parseFloat(st.nextToken());
            blue = Float.parseFloat(st.nextToken());
        } catch (Exception nfex) {
            System.out.println(nfex.toString());
        }

        if (st.hasMoreTokens()) {
            float alpha = Float.parseFloat(st.nextToken());

            lutVector.add(new LUValue(red, green, blue, alpha));
        } else {
            lutVector.add(new LUValue(red, green, blue));
        }
    }

    /**
     * Adds a point to the function associated with the LUT or modelRGB.
     * 
     * @param s String
     */
    private void addPointToFunction(String s) {
        float x = 0;
        float y = 0;
        StringTokenizer st = new StringTokenizer(s, ",");

        try {
            x = Float.parseFloat(st.nextToken());
            y = Float.parseFloat(st.nextToken());
        } catch (NumberFormatException nfex) {
            System.out.println(nfex.toString());
        }

        functionVector.addElement(new Vector2f(x, y));
    }

    /**
     * Returns a String for writing the thumbnail data (r,g,b) to the XML header.
     * 
     * @param data int[] array of int thumbnail data
     * @param index int index of thumbnail data (which line)
     * 
     * @return String a formatted string containing comma separated thumbnail information
     */
    private String getFormattedThumbnailLine(int[] data, int index) {

        String temp = "";

        for (int i = 0; (i < 6) && (index < data.length); i++, index += 4) {
            temp += Integer.toString(data[index + 1]) + ","; // red
            temp += Integer.toString(data[index + 2]) + ","; // green
            temp += Integer.toString(data[index + 3]) + ",";
        }

        // chop off that last comma
        return temp.substring(0, temp.length() - 1);
    }

    /**
     * Sets up the LUT based on the values read into the lutVector.
     */
    private void setLUT() {

        // System.err.println("start of setLUT() of FileXML");
        if (LUT == null) {
            LUT = new ModelLUT(ModelLUT.GRAY, 256, new int[] {4, 256});
        }

        int height = LUT.getExtents()[1];

        // now import the LUT
        LUValue lv;
        int m;

        for (m = 0; m < height; m++) {
            lv = lutVector.elementAt(m);
            Preferences.debug(TAB + m + " alpha: " + lv.alpha + ", red: " + lv.red + ", green: " + lv.green
                    + ", blue: " + lv.blue + "\n", Preferences.DEBUG_FILEIO);

            LUT.setColor(m, (int) lv.alpha, (int) lv.red, (int) lv.green, (int) lv.blue);
        }

    }

    /**
     * sets up the model RGB while parsing XML header after all LUValues have been saved in the LUT vector.
     */
    private void setRGB() {

        // System.err.println("start of setRGB() of FileXML");
        if (modelRGB == null) {
            modelRGB = new ModelRGB(new int[] {4, 256});
        }

        int height = modelRGB.getExtents()[1];

        // now import the LUT
        LUValue lv;
        int m;

        for (m = 0; m < height; m++) {
            lv = lutVector.elementAt(m);
            Preferences.debug(TAB + m + " alpha: " + lv.alpha + ", red: " + lv.red + ", green: " + lv.green
                    + ", blue: " + lv.blue + "\n", Preferences.DEBUG_FILEIO);
            modelRGB.set(0, m, lv.alpha);
            modelRGB.set(1, m, lv.red);
            modelRGB.set(2, m, lv.green);
            modelRGB.set(3, m, lv.blue);
        }

    }

    /**
     * Updates the start locations. Each image has a fileinfo where the start locations are stored. Note that the start
     * location for the Z (3rd) dimension change with the change is the slice. The origin is in the upper left corner
     * and we are using the right hand rule. + x -> left to right; + y -> top to bottom and + z -> into screen.
     * 
     * @param xmlInfo the file info to update
     */
    private void updateOriginInfo(FileInfoImageXML[] xmlInfo) {
        int axisOrient;

        float[] origin = xmlInfo[0].getOrigin();
        float[] resolutions = xmlInfo[0].getResolutions();

        if (image.getNDims() == 3) {

            for (int i = 0; i < image.getExtents()[2]; i++) {
                xmlInfo[i].setOrigin(origin);
                axisOrient = xmlInfo[i].getAxisOrientation(2);

                if ( (axisOrient == FileInfoBase.ORI_R2L_TYPE) || (axisOrient == FileInfoBase.ORI_P2A_TYPE)
                        || (axisOrient == FileInfoBase.ORI_I2S_TYPE) || (axisOrient == FileInfoBase.ORI_UNKNOWN_TYPE)) {
                    origin[2] += resolutions[2];
                } else { // ORI_L2R_TYPE, ORI_A2P_TYPE, ORI_S2I_TYPE
                    origin[2] -= resolutions[2];
                }
            }
        } else if (image.getNDims() == 4) {
            float tmp = origin[2];

            for (int i = 0; i < image.getExtents()[3]; i++) {

                for (int j = 0; j < image.getExtents()[2]; j++) {
                    xmlInfo[ (i * image.getExtents()[2]) + j].setOrigin(origin);
                    axisOrient = xmlInfo[i].getAxisOrientation(2);

                    if ( (axisOrient == FileInfoBase.ORI_R2L_TYPE) || (axisOrient == FileInfoBase.ORI_P2A_TYPE)
                            || (axisOrient == FileInfoBase.ORI_I2S_TYPE)) {
                        origin[2] += resolutions[2];
                    } else { // ORI_L2R_TYPE, ORI_A2P_TYPE, ORI_S2I_TYPE
                        origin[2] -= resolutions[2];
                    }
                }

                origin[3] += resolutions[3];
                origin[2] = tmp;
            }
        }
    }

    /**
     * Method to save off the header from a 4D image into 3D header files sequentially named (similar to the method in
     * FileRaw).
     * 
     * @param img Image to be saved
     * @param headerName Name of file
     * @param headerDir Directory for file
     * @param options File write options (contains # of digits and start #)
     * 
     * @throws IOException DOCUMENT ME!
     */
    private void writeHeader3DTo2D(ModelImage img, String headerName, String headerDir, FileWriteOptions options)
            throws IOException {
        int k, seq;
        int beginSlice = options.getBeginSlice();
        int endSlice = options.getEndSlice();
        String origName = new String(headerName);

        for (k = beginSlice, seq = options.getStartNumber(); k <= endSlice; k++, seq++) {
            headerName = origName;

            if (options.getDigitNumber() == 1) {
                headerName += Integer.toString(seq);
            } else if (options.getDigitNumber() == 2) {

                if (seq < 10) {
                    headerName += "0" + Integer.toString(seq);
                } else {
                    headerName += Integer.toString(seq);
                }
            } else if (options.getDigitNumber() == 3) {

                if (seq < 10) {
                    headerName += "00" + Integer.toString(seq);
                } else if (seq < 100) {
                    headerName += "0" + Integer.toString(seq);
                } else {
                    headerName += Integer.toString(seq);
                }
            } else if (options.getDigitNumber() == 4) {

                if (seq < 10) {
                    headerName += "000" + Integer.toString(seq);
                } else if (seq < 100) {
                    headerName += "00" + Integer.toString(seq);
                } else if (seq < 1000) {
                    headerName += "0" + Integer.toString(seq);
                } else {
                    headerName += Integer.toString(seq);
                }
            }

            writeHeader(img, options, headerName, headerDir, true);

        } // end for loop
    }

    /**
     * Method to save off the header from a 4D image into 3D header files sequentially named (similar to the method in
     * FileRaw).
     * 
     * @param img Image to be saved
     * @param headerName name of file
     * @param headerDir directory for file
     * @param options file write options
     * 
     * @throws IOException DOCUMENT ME!
     */
    private void writeHeader4DTo3D(ModelImage img, String headerName, String headerDir, FileWriteOptions options)
            throws IOException {
        int k, seq;
        int beginTime = options.getBeginTime();
        int endTime = options.getEndTime();
        String origName = new String(headerName);

        for (k = beginTime, seq = options.getStartNumber(); k <= endTime; k++, seq++) {
            headerName = origName;

            if (options.getDigitNumber() == 1) {
                headerName += Integer.toString(seq);
            } else if (options.getDigitNumber() == 2) {

                if (seq < 10) {
                    headerName += "0" + Integer.toString(seq);
                } else {
                    headerName += Integer.toString(seq);
                }
            } else if (options.getDigitNumber() == 3) {

                if (seq < 10) {
                    headerName += "00" + Integer.toString(seq);
                } else if (seq < 100) {
                    headerName += "0" + Integer.toString(seq);
                } else {
                    headerName += Integer.toString(seq);
                }
            } else if (options.getDigitNumber() == 4) {

                if (seq < 10) {
                    headerName += "000" + Integer.toString(seq);
                } else if (seq < 100) {
                    headerName += "00" + Integer.toString(seq);
                } else if (seq < 1000) {
                    headerName += "0" + Integer.toString(seq);
                } else {
                    headerName += Integer.toString(seq);
                }
            }

            writeHeader(img, options, headerName, headerDir, true);

        } // end for loop
    }

    /**
     * <code>writeSet</code> is a helper method, to allow writing set data out to the <code>BufferedWriter</code>
     * with any enumeration.
     * 
     * @param bw The writer to which we will write the data.
     * @param setEnum An enumerated list of set data.
     */
    @SuppressWarnings("unchecked")
    private void writeSet(BufferedWriter bw, Enumeration<XMLPSet> setEnum) {

        boolean openTagFalseFlag = false; // Flag to specify whether end tag is needed.

        while (setEnum.hasMoreElements()) {
            XMLPSet currentSet = setEnum.nextElement();
            Enumeration<XMLParameter> paramEnum = currentSet.getTable().elements();
            openTagFalseFlag = false;

            // Write set description only if paraEnum has atleast one parameter.
            if (paramEnum.hasMoreElements()) {

                openTagFalseFlag = true; // End tag required.
                openTag(imageStr[4], true);

                String temp = currentSet.getDescription();

                closedTag(setStr[0], temp);
            }

            while (paramEnum.hasMoreElements()) {
                XMLParameter currentParam = paramEnum.nextElement();

                // if (!(currentParam.getValue() == null) &&
                // !(currentParam.getValue().equals("")))
                // {
                openTag(setStr[1], true); // begin parameter

                // write in Parameter NAME
                closedTag(parameterStr[0], currentParam.getName());

                // Parameter DESCRIPTION
                String temp2 = currentParam.getDescription();

                if ( (temp2 != null) && !temp2.equalsIgnoreCase("")) {
                    closedTag(parameterStr[1], temp2);
                }

                // parameter VALUE TYPE
                closedTag(parameterStr[2], currentParam.getValueType());

                // Parameter VALUE
                closedTag(parameterStr[3], currentParam.getValue());

                // Parameter DATE and TIME
                if ( (currentParam.getDate() != null) && (currentParam.getDate() != null)) {
                    closedTag(parameterStr[4], currentParam.getDate() + "T" + currentParam.getTime());
                }

                openTag(setStr[1], false); // end Parameter

                // }
            }
            // If true end tag required.
            if (openTagFalseFlag) {
                openTag(imageStr[4], false);
            }

        }
    }

    // ~ Inner Classes
    // --------------------------------------------------------------------------------------------------

    /**
     * Holds an AWT Image and an int[] array of data for an XML Thumbnail image the thumbnail is stored in the XML's
     * header so that it may be read in independently from the actual image data. when an XML file is saved, the user
     * has the option to also saved a reduced resolution (max 64x64) image into the XML's header using standard XML
     * tagging
     * 
     * @author Ben Link
     */
    public class Thumbnail extends JComponent implements Serializable {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 6645327578121170860L;

        /** DOCUMENT ME! */
        private int brightness = 0;

        /** DOCUMENT ME! */
        private float contrast = 1f;

        @SuppressWarnings("unused")
        private int[] data;

        /** DOCUMENT ME! */
        private Image img;

        /** DOCUMENT ME! */
        private int imgWidth, imgHeight; // initially set to width and height of image...rescales based on panel size

        /** DOCUMENT ME! */
        private MemoryImageSource memImage = null;

        /**
         * Default constructor for the Thumbnail.
         * 
         * @param xDim Width of image
         * @param yDim Height of image
         * @param data actual image data (stored in ARGB packed int format)
         */
        public Thumbnail(int xDim, int yDim, int[] data) {
            this.data = data;
            this.imgWidth = xDim;
            this.imgHeight = yDim;
            memImage = new MemoryImageSource(xDim, yDim, data, 0, xDim);
            img = createImage(new FilteredImageSource(memImage, new ViewJFilterAnimate(brightness, contrast)));
        }

        /**
         * Cleans up the thumbnail AWT Image and int[] array.
         */
        public void finalize() {
            data = null;

            if (img != null) {
                img.flush();
                img = null;
            }

            memImage = null;
        }

        /**
         * Gets the preferred size based on the parent (container's) size.
         * 
         * @return the preferred size of the thumbnail
         */
        public Dimension getPreferredSize() {
            Dimension size = null;

            size = this.getParent().getSize();

            setImgSize(size.width, size.height);

            try {
                return new Dimension(imgWidth, imgHeight);
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: ComponentBase.getPreferredSize");

                return null;
            }
        }

        /**
         * Draws the component.
         * 
         * @param g graphics g
         */
        public void paintComponent(Graphics g) {
            g.setClip(getVisibleRect());

            // setBounds( (c.getWidth() - imgWidth) / 2, (c.getHeight() - imgHeight) / 2, imgWidth, imgHeight);
            g.drawImage(img, 0, 0, imgWidth, imgHeight, 0, 0, img.getWidth(this), img.getHeight(this), null);
        }

        /**
         * Sets the brightness and contrast levels for the image.
         * 
         * @param brightness the thumbnail brightness
         * @param contrast the thumbnail contrast
         */
        public void setBrightnessContrast(int brightness, float contrast) {
            this.brightness = brightness;
            this.contrast = contrast;

            if (img != null) {
                img.flush();
                img = null;
            }

            img = createImage(new FilteredImageSource(memImage, new ViewJFilterAnimate(brightness, contrast)));

            if (getGraphics() != null) {
                paintComponent(getGraphics());
            }
        }

        /**
         * Sets the image size based on the panel that is displaying it (its parent).
         * 
         * @param width panel width
         * @param height panel height
         */
        public void setImgSize(int width, int height) {
            int panelWidth = width - 10;
            int panelHeight = height - 10;
            int w = img.getWidth(this);
            int h = img.getHeight(this);

            // find fraction x such that w*x = width
            float fracX = (float) panelWidth / w;

            // find fraction y such that height*y = 200
            float fracY = (float) panelHeight / h;

            // min(fractX, fractY) will size image within 400 x 200 rectangle
            float min = (fracX < fracY) ? fracX : fracY;

            imgWidth = Math.round(min * w);
            imgHeight = Math.round(min * h);
        }
    }

    /**
     * Stores LUT information (red, green, blue, alpha).
     */
    private class LUValue {

        @SuppressWarnings("unused")
        private boolean hasAlpha = false;

        /** DOCUMENT ME! */
        private float red, green, blue, alpha;

        /**
         * Creates a new LUValue object.
         * 
         * @param red DOCUMENT ME!
         * @param green DOCUMENT ME!
         * @param blue DOCUMENT ME!
         */
        public LUValue(float red, float green, float blue) {
            this.red = red;
            this.green = green;
            this.blue = blue;
        }

        /**
         * Creates a new LUValue object.
         * 
         * @param alpha DOCUMENT ME!
         * @param red DOCUMENT ME!
         * @param green DOCUMENT ME!
         * @param blue DOCUMENT ME!
         */
        public LUValue(float alpha, float red, float green, float blue) {
            this.red = red;
            this.green = green;
            this.blue = blue;
            this.alpha = alpha;
            this.hasAlpha = true;
        }
    }

    /**
     * Used by the XML Parser to parse the xml header.
     */
    private class MyXMLHandler extends DefaultHandler {

        /** DOCUMENT ME! */
        float[] acpcMax = null;

        /** DOCUMENT ME! */
        int acpcMaxCount = -1;
        
        int gradCount = -1;

        /** DOCUMENT ME! */
        float[] acpcMin = null;

        /** DOCUMENT ME! */
        int acpcMinCount = -1;

        /** DOCUMENT ME! */
        float[] acpcPC = null;

        /** DOCUMENT ME! */
        int acpcPCCount = -1;

        /** DOCUMENT ME! */
        Vector<VOI> annotationVector;

        /** DOCUMENT ME! */
        VOI annotationVOI;

        /** DOCUMENT ME! */
        int axisCount = -1;

        /** DOCUMENT ME! */
        String currentKey;

        /** DOCUMENT ME! */
        String elementBuffer = new String();

        /** DOCUMENT ME! */
        int extentsCount = -1;

        /** DOCUMENT ME! */
        FileInfoImageXML fileInfo;

        /** DOCUMENT ME! */
        float[] firstResolutions = null;

        /** DOCUMENT ME! */
        boolean isColor = false;

        Vector<TransMatrix> matrixVector = null;

        /** DOCUMENT ME! */
        TransMatrix matrix;

        /** DOCUMENT ME! */
        int matrixCol = -1;

        /** DOCUMENT ME! */
        int matrixRow = -1;

        /** DOCUMENT ME! */
        int nDimensions;

        /** DOCUMENT ME! */
        int numInvestigators;

        /** DOCUMENT ME! */
        int numParameters;

        /** DOCUMENT ME! */
        float[] origAC = null;

        /** DOCUMENT ME! */
        int origACCount = -1;

        /** DOCUMENT ME! */
        int[] origDim = null;

        /** DOCUMENT ME! */
        int origDimCount = -1;
        
        float[] origOrigin = null;
        
        int origOriginCount = -1;

        /** DOCUMENT ME! */
        float[] origOrientation = null;

        /** DOCUMENT ME! */
        int origOrientationCount = -1;

        /** DOCUMENT ME! */
        float[] origPC = null;

        /** DOCUMENT ME! */
        int origPCCount = -1;

        /** DOCUMENT ME! */
        float[] origRes = null;

        /** DOCUMENT ME! */
        int origResCount = -1;

        /** DOCUMENT ME! */
        int resInfoCount = -1;

        /** DOCUMENT ME! */
        float[][] resolutions = null;

        /** DOCUMENT ME! */
        int resolutionsCount = -1;

        /** DOCUMENT ME! */
        int startCount = -1;

        /** DOCUMENT ME! */
        TalairachTransformInfo talairach;
        
       

        /** DOCUMENT ME! */
        int[] thumbnailBuffer = null;

        /** DOCUMENT ME! */
        int thumbnailBufferIndex = 0; // index into the byte buffer

        /** DOCUMENT ME! */
        int thumbnailXDim = 64;

        /** DOCUMENT ME! */
        int thumbnailYDim = 64;

        /** DOCUMENT ME! */
        float[] tlrcRes = null;

        /** DOCUMENT ME! */
        int tlrcResCount = -1;

        /** DOCUMENT ME! */
        int unitsCount = -1;

        /**
         * Creates a new MyXMLHandler object.
         * 
         * @param fInfo DOCUMENT ME!
         * @param hisVector DOCUMENT ME!
         * @param anVector DOCUMENT ME!
         * @param tal DOCUMENT ME!
         */ 
        public MyXMLHandler(FileInfoImageXML fInfo, Vector<VOI> anVector, Vector<TransMatrix> mVector,
                TalairachTransformInfo tal) {
            fileInfo = fInfo;   
            annotationVector = anVector;
            matrixVector = mVector;
            this.talairach = tal;
        }

        /**
         * Text data callback from parser. If the parser is not validating, this method can report whitespace. We ignore
         * strings that are entirely whitespace.
         * 
         * @param ch Character array
         * @param start Start of data in array.
         * @param length Length of data in array.
         */
        public void characters(char[] ch, int start, int length) {
            String s = new String(ch, start, length);

            // don't need to de-entity-ize the string because the parser does that automatically

            if (s.trim().length() != 0) {
                elementBuffer += s;
            }
        }

        /**
         * Do nothing.
         * 
         * @throws SAXException never happens
         */
        public void endDocument() throws SAXException {}

        /**
         * Called by parser when the end of an element is reached in the document.
         * 
         * @param namespaceURI the namespace uri
         * @param localName the element name
         * @param qName the qualified name
         * 
         * @throws SAXException if a problem is encountered during parsing
         */
        public void endElement(String namespaceURI, String localName, String qName) throws SAXException {
            currentKey = localName;

            if (currentKey.equals("Data-type")) {
                Preferences.debug("Data-type: " + elementBuffer + "\n", Preferences.DEBUG_FILEIO);
                fileInfo.setDataType(FileInfoBase.getDataTypeFromStr(elementBuffer));

                if ( (fileInfo.getDataType() == ModelStorageBase.ARGB)
                        || (fileInfo.getDataType() == ModelStorageBase.ARGB_FLOAT)
                        || (fileInfo.getDataType() == ModelStorageBase.ARGB_USHORT)) {
                    isColor = true;
                }
            } else if (currentKey.equals("Description")) {
                Preferences.debug("Description: " + elementBuffer + "\n", Preferences.DEBUG_FILEIO);
                fileInfo.setImageDescription(elementBuffer);
            } else if (currentKey.equals("History")) {
                Preferences.debug("History: " + elementBuffer + "\n", Preferences.DEBUG_FILEIO);
                fileInfo.setHistory(elementBuffer);
            } else if (currentKey.equals("Linked-image")) {

                if (new File(elementBuffer).exists()) {
                    fileInfo.setLinkedImagePath(elementBuffer);
                    Preferences.debug("Linked-image: " + elementBuffer + "\n", Preferences.DEBUG_FILEIO);
                    // System.err.println("PATH: " + elementBuffer);
                } else {
                    Preferences.debug("Linked-image path not valid, removing\n", Preferences.DEBUG_FILEIO);
                }
            } else if (currentKey.equals("Endianess")) {
                Preferences.debug("Endianess: " + elementBuffer + "\n", Preferences.DEBUG_FILEIO);
                fileInfo.setEndianess(FileInfoBase.getEndianessFromStr(elementBuffer));
            } else if (currentKey.equals("Extents")) {
                extentsCount++;
                Preferences.debug("Extents: " + elementBuffer + "\n", Preferences.DEBUG_FILEIO);
                fileInfo.setExtents(Integer.valueOf(elementBuffer).intValue(), extentsCount);
            } else if (currentKey.equals("Units")) {
                unitsCount++;
                Preferences.debug("Units " + unitsCount + ": " + elementBuffer + "\n", Preferences.DEBUG_FILEIO);
                fileInfo.setUnitsOfMeasure(Unit.getUnit(elementBuffer), unitsCount);
            } else if (currentKey.equals("Compression")) {
                Preferences.debug("Compression: " + elementBuffer + "\n", Preferences.DEBUG_FILEIO);

                if (elementBuffer.equals("none")) {
                    fileInfo.setCompressionType(FileInfoBase.COMPRESSION_NONE);
                } else if (elementBuffer.equals("zipped")) {
                    fileInfo.setCompressionType(FileInfoBase.COMPRESSION_ZIP);
                }
            } else if (currentKey.equals("Resolution")) {
                resolutionsCount++;

                if (resInfoCount == 0) {
                    firstResolutions[resolutionsCount] = Float.valueOf(elementBuffer).floatValue();
                } else {
                    resolutions[resolutionsCount][resInfoCount] = Float.valueOf(elementBuffer).floatValue();
                }
            } else if (currentKey.equals("Slice-spacing")) {
                Preferences.debug("Slice spacing (res[2]): " + elementBuffer + "\n", Preferences.DEBUG_FILEIO);
                fileInfo.setResolutions(Float.valueOf(elementBuffer).floatValue(), 2);
            } else if (currentKey.equals("Slice-thickness")) {
                Preferences.debug("Slice thickness: " + elementBuffer + "\n", Preferences.DEBUG_FILEIO);
                fileInfo.setSliceThickness(Float.valueOf(elementBuffer).floatValue());
            } else if (currentKey.equals("Orientation")) {
                Preferences.debug("Orientation: " + elementBuffer + "\n", Preferences.DEBUG_FILEIO);
                fileInfo.setImageOrientation(FileInfoBase.getImageOrientationFromStr(elementBuffer));
            } else if (currentKey.equals("Subject-axis-orientation")) {
                axisCount++;
                Preferences.debug("Axis " + axisCount + ": " + elementBuffer + "\n", Preferences.DEBUG_FILEIO);
                fileInfo.setAxisOrientation(FileInfoBase.getAxisOrientationFromStr(elementBuffer), axisCount);
            } else if (currentKey.equals("Origin")) {
                startCount++;
                Preferences.debug("Origin " + startCount + ": " + elementBuffer + "\n", Preferences.DEBUG_FILEIO);
                fileInfo.setOrigin(Float.valueOf(elementBuffer).floatValue(), startCount);
            } else if (currentKey.equals("Modality")) {
                Preferences.debug("Modality: " + elementBuffer + "\n", Preferences.DEBUG_FILEIO);
                fileInfo.setModality(FileInfoBase.getModalityFromStr(elementBuffer));
            } else if (currentKey.equals("Transform-ID")) {
                Preferences.debug("Transform ID: " + elementBuffer + "\n", Preferences.DEBUG_FILEIO);
                matrix.setTransformID(TransMatrix.getTransformIDFromStr(elementBuffer));
            } else if (currentKey.equals("Data")) {
                matrixCol++;

                if ( (matrixCol % matrix.getDim()) == 0) {
                    matrixCol = 0;
                    matrixRow++;
                }

                try {
                    matrix.set(matrixRow, matrixCol, Double.valueOf(elementBuffer).doubleValue());
                } catch (ArrayIndexOutOfBoundsException ex) {
                    // ex.printStackTrace();
                }
            } else if (currentKey.equals("Image-offset")) {
                Preferences.debug("Image-offset: " + elementBuffer + "\n", Preferences.DEBUG_FILEIO);
                fileInfo.setOffset(Integer.valueOf(elementBuffer).intValue());
            } else if (currentKey.equals("Subject-name")) {
                fileInfo.setSubjectName(elementBuffer);
            } else if (currentKey.equals("Race")) {
                fileInfo.setRace(elementBuffer);
            } else if (currentKey.equals("Subject-ID")) {
                fileInfo.setSubjectID(elementBuffer);
            } else if (currentKey.equals("Diagnosis")) {
                fileInfo.setDiagnosis(elementBuffer);
            } else if (currentKey.equals("Date-of-birth")) {
                fileInfo.setDOB(elementBuffer);
            } else if (currentKey.equals("Height")) {
                fileInfo.setHeight(Integer.valueOf(elementBuffer).intValue());
            } else if (currentKey.equals("Weight")) {
                fileInfo.setWeight(Integer.valueOf(elementBuffer).intValue());
            } else if (currentKey.equals("Sex")) {
                fileInfo.setSex(elementBuffer);
            } else if (currentKey.equals("Body-part")) {
                fileInfo.setBodyPart(elementBuffer);
            } else if (currentKey.equals("Equipment-model-name")) {
                fileInfo.setEquipmentName(elementBuffer);
            } else if (currentKey.equals("Scan-ID")) {
                fileInfo.setScanID(elementBuffer);
            } else if (currentKey.equals("Protocol")) {
                fileInfo.setProtocol(elementBuffer);
            } else if (currentKey.equals("Scan-date-time")) {
                fileInfo.setScanDateTime(elementBuffer);
            } else if (currentKey.equals("Investigator-name")) {
                numInvestigators++;
                fileInfo.setInvestigatorName(elementBuffer, numInvestigators);
            } else if (currentKey.equals("Title")) {
                fileInfo.setTitle(elementBuffer, numInvestigators);
            } else if (currentKey.equals("Affiliation")) {
                fileInfo.setAffiliation(elementBuffer, numInvestigators);
            } else if (currentKey.equals("Email")) {
                fileInfo.setEmail(elementBuffer, numInvestigators);
            } else if (currentKey.equals("Phone")) {
                fileInfo.setPhone(elementBuffer, numInvestigators);
            } else if (currentKey.equals("Set-description")) {

                // System.out.println("Found SET: " + elementBuffer + "\n");
                fileInfo.createPSet(elementBuffer);
            } else if (currentKey.equals("Parameter-name")) {
                numParameters++;
                fileInfo.getCurrentPSet().addParameter(elementBuffer);
            } else if (currentKey.equals("Parameter-description")) {
                fileInfo.getCurrentPSet().getCurrentParameter().setDescription(elementBuffer);
            } else if (currentKey.equals("Value-type")) {
                fileInfo.getCurrentPSet().getCurrentParameter().setValueType(elementBuffer);
            } else if (currentKey.equals("Value")) {
                fileInfo.getCurrentPSet().getCurrentParameter().setValue(elementBuffer);
            } else if (currentKey.equals("Parameter-date-time")) {
                fileInfo.getCurrentPSet().getCurrentParameter().setDateTime(elementBuffer);
            } else if (currentKey.equals("Function-type")) {

                // System.out.println("Function-Type: " + elementBuffer);
                if (elementBuffer.equals("transfer")) {
                    functionIndex = 0;
                } else if (elementBuffer.equals("alpha")) {
                    functionIndex = 1;
                } else if (elementBuffer.equals("red")) {
                    functionIndex = 2;
                } else if (elementBuffer.equals("green")) {
                    functionIndex = 3;
                } else if (elementBuffer.equals("blue")) {
                    functionIndex = 4;
                }
            } else if (currentKey.equals("Point")) {
                addPointToFunction(elementBuffer);
            } else if (currentKey.equals("LUValue")) {
                addLUValue(elementBuffer);
            } else if (currentKey.equals("LUT-functions")) {

                if (isColor) {
                    addFunctionToRGB();
                } else {
                    addFunctionToLUT();
                }

                functionVector.removeAllElements();
            } else if (currentKey.equals("LUT")) {

                if (isColor) {
                    setRGB();
                } else {
                    setLUT();
                }

                lutVector.clear();
            } else if (currentKey.equals("VOI-path")) {
                fileInfo.addVOI(elementBuffer);
            } else if (currentKey.equals("Load-VOI-with-image")) {
                fileInfo.getCurrentVOI().setDisplay(Boolean.valueOf(elementBuffer).booleanValue());
            } else if (currentKey.equals("Surface-path")) {
                fileInfo.addSurface(elementBuffer);
            } else if (currentKey.equals("Load-surface-with-image")) {
                fileInfo.getCurrentSurface().setDisplay(Boolean.valueOf(elementBuffer).booleanValue());
            } else if (currentKey.equals("Surface-opacity")) {
                fileInfo.getCurrentSurface().setOpacity(Float.valueOf(elementBuffer).floatValue());
            } else if (currentKey.equals("Thumbnail-data")) {

                // System.err.println("got thumbnail data");
                StringTokenizer stok = new StringTokenizer(elementBuffer, ",");

                while (stok.hasMoreTokens()) {
                    thumbnailBuffer[thumbnailBufferIndex] = 0xff000000;
                    thumbnailBuffer[thumbnailBufferIndex] |= (Integer.parseInt(stok.nextToken()) << 16); // red
                    thumbnailBuffer[thumbnailBufferIndex] |= (Integer.parseInt(stok.nextToken()) << 8); // green
                    thumbnailBuffer[thumbnailBufferIndex] |= Integer.parseInt(stok.nextToken()); // blue
                    thumbnailBufferIndex++;
                }
            } else if (currentKey.equals("image")) {

                // at the end of the XML document we want to copy over the thumbnail buffer (if it isn't null)
                // System.err.println("End thumbnailBuffer Index: " + thumbnailBufferIndex);
                if (thumbnailBuffer != null) {
                    setThumbnailData(thumbnailXDim, thumbnailYDim, thumbnailBuffer);

                }
            } else if (currentKey.equals("Text-location")) {

                if (annotationVOI != null) {
                    StringTokenizer stok = new StringTokenizer(elementBuffer, ",");
                    float[] x = new float[1];
                    float[] y = new float[1];
                    float[] z = new float[1];
                    x[0] = Float.parseFloat(stok.nextToken());
                    y[0] = Float.parseFloat(stok.nextToken());
                    z[0] = Integer.parseInt(stok.nextToken());
                    annotationVOI.importCurve(x, y, z);
                    annotationVOI.setUID(annotationVOI.hashCode());
                }
            } else if (currentKey.equals("Text")) {
                ((VOIText) annotationVOI.getCurves().lastElement()).setText(elementBuffer);
            } else if (currentKey.equals("Font-color")) {
                StringTokenizer stok = new StringTokenizer(elementBuffer, ",");
                Color tempColor = new Color(Integer.parseInt(stok.nextToken()), Integer.parseInt(stok.nextToken()),
                        Integer.parseInt(stok.nextToken()));
                annotationVOI.setColor(tempColor);
                ((VOIText) annotationVOI.getCurves().lastElement()).setColor(tempColor);

            } else if (currentKey.equals("Font-descriptor")) {
                ((VOIText) annotationVOI.getCurves().lastElement()).setFontDescriptors(Integer
                        .parseInt(elementBuffer));
            } else if (currentKey.equals("Font-size")) {
                ((VOIText) annotationVOI.getCurves().lastElement()).setFontSize(Integer
                        .parseInt(elementBuffer));
            } else if (currentKey.equals("Font-type")) {
                ((VOIText) annotationVOI.getCurves().lastElement()).setFontName(elementBuffer);

                // add this VOI to the vector
                annotationVector.addElement(annotationVOI);
            } else if (currentKey.equals("origAC")) {
                origACCount++;
                origAC[origACCount] = Float.parseFloat(elementBuffer);

                if (origACCount == 2) {
                    talairach.setOrigAC(new Vector3f(origAC[0], origAC[1], origAC[2]));
                }
            } else if (currentKey.equals("origPC")) {
                origPCCount++;
                origPC[origPCCount] = Float.parseFloat(elementBuffer);

                if (origPCCount == 2) {
                    talairach.setOrigPC(new Vector3f(origPC[0], origPC[1], origPC[2]));
                }
            } else if (currentKey.equals("acpcPC")) {
                acpcPCCount++;
                acpcPC[acpcPCCount] = Float.parseFloat(elementBuffer);

                if (acpcPCCount == 2) {
                    talairach.setAcpcPC(new Vector3f(acpcPC[0], acpcPC[1], acpcPC[2]));
                }
            } else if (currentKey.equals("origDim")) {
                origDimCount++;
                origDim[origDimCount] = Integer.parseInt(elementBuffer);

                if (origDimCount == 2) {
                    talairach.setOrigDim(origDim);
                }
            } else if (currentKey.equals("origOrigin")) {
                origOriginCount++;
                origOrigin[origOriginCount] = Float.parseFloat(elementBuffer);

                if (origOriginCount == 2) {
                    talairach.setOrigOrigin(origOrigin);
                }
            } else if (currentKey.equals("origRes")) {
                origResCount++;
                origRes[origResCount] = Float.parseFloat(elementBuffer);

                if (origResCount == 2) {
                    talairach.setOrigRes(origRes);
                }
            } else if (currentKey.equals("origOrient")) {
                origOrientationCount++;
                origOrientation[origOrientationCount] = Float.parseFloat(elementBuffer);

                if (origOrientationCount == 8) {
                    float[][] R = new float[3][3];

                    for (int i = 0; i < 3; i++) {

                        for (int j = 0; j < 3; j++) {
                            R[i][j] = origOrientation[ (i * 3) + j];
                        }
                    }

                    talairach.setOrigOrient(R);
                }
            } else if (currentKey.equals("acpcMin")) {
                acpcMinCount++;
                acpcMin[acpcMinCount] = Float.parseFloat(elementBuffer);

                if (acpcMinCount == 2) {
                    talairach.setAcpcMin(new Vector3f(acpcMin[0], acpcMin[1], acpcMin[2]));
                }
            } else if (currentKey.equals("acpcMax")) {
                acpcMaxCount++;
                acpcMax[acpcMaxCount] = Float.parseFloat(elementBuffer);

                if (acpcMaxCount == 2) {
                    talairach.setAcpcMax(new Vector3f(acpcMax[0], acpcMax[1], acpcMax[2]));
                }
            } else if (currentKey.equals("acpcRes")) {
                talairach.setAcpcRes(Float.parseFloat(elementBuffer));
            } else if (currentKey.equals("tlrcRes")) {
                tlrcResCount++;
                tlrcRes[tlrcResCount] = Float.parseFloat(elementBuffer);

                if (tlrcResCount == 6) {
                    talairach.setTlrcRes(tlrcRes);
                }
            } else if (currentKey.equals("Matrix")) {
                // add the current matrix to the vector
                if (matrix != null) {
                    matrixVector.add(matrix);
                }
            } else if (currentKey.equals("volumeNum")){
                    dtiparams = new DTIParameters(Integer.parseInt(elementBuffer));
                    numVolumes = Integer.parseInt(elementBuffer);
                    dtiparams.setNumVolumes(Integer.parseInt(elementBuffer));
                
            }
              else if (currentKey.equals("bValues")){
                  bValues = new float[numVolumes];
                  for (int i = 0; i < numVolumes; i++) {                     
                      String[] arr = elementBuffer.split("/");
                      bValues[i] = Float.parseFloat(arr[i]);
                  }
                  dtiparams.setbValues(bValues);
                
            } else if (currentKey.equals("gradient")){
                gradCount++;
                String[] arr = elementBuffer.split(",");
                gradients[gradCount][0] = Float.parseFloat(arr[0]);
                gradients[gradCount][1] = Float.parseFloat(arr[1]);
                gradients[gradCount][2] = Float.parseFloat(arr[2]); 
            }
            
        }

        /**
         * Accessor to return the resolutions per slice as parsed from the XML header.
         * 
         * @return double array containing resolutions of each slice
         */
        public float[][] getResolutions() {

            if (resolutions == null) {
                resolutions = new float[nDimensions][1];

                for (int i = 0; i < nDimensions; i++) {
                    resolutions[i][0] = firstResolutions[i];
                    // System.err.println(resolutions[i][0]);
                }
                // System.err.println("Returning new [] resolutions, length " + resolutions[0].length);
            }

            return resolutions;
        }

        /**
         * Do nothing but show the entity we skipped.
         * 
         * @param name the skipped entity name
         */
        public void skippedEntity(String name) {
            System.out.println(name);
        }

        /**
         * Parser calls this for the beginning of each element in the document.
         * 
         * @param namespaceURI the namespace uri
         * @param localName the element name
         * @param qName the qualified name
         * @param atts the attached attributes
         * 
         * @throws SAXException if a problem is encountered during parsing
         */
        public void startElement(String namespaceURI, String localName, String qName, Attributes atts)
                throws SAXException {
            currentKey = localName;

            if (currentKey.equals("image")) {

                // Note: these don't have to be in this order, should use another method
                imageFileName = atts.getValue("filename");
                fileInfo.setImageDataFileName(imageFileName);

                // System.out.println("Image file name: " + imageFileName);
                nDimensions = Integer.valueOf(atts.getValue("nDimensions")).intValue();
                Preferences.debug("FileXML: nDimensions = " + TAB + nDimensions + "\n", Preferences.DEBUG_FILEIO);
                fileInfo.setExtents(new int[nDimensions]);
                fileInfo.setResolutions(new float[nDimensions]);
                fileInfo.setSliceThickness((float) 0.0);

                fileInfo.setMatrix(matrix);
            } else if (currentKey.equals("Matrix")) {
                if (nDimensions == 2) {
                    matrix = new TransMatrix(3);
                } else {
                    matrix = new TransMatrix(4);
                }
                matrixCol = -1;
                matrixRow = -1;
            } else if (currentKey.equals("Thumbnail")) {
                thumbnailXDim = Integer.parseInt(atts.getValue("xDim"));
                thumbnailYDim = Integer.parseInt(atts.getValue("yDim"));

                // allocate a new buffer for the thumbnail (xDim * yDim))
                thumbnailBuffer = new int[thumbnailXDim * thumbnailYDim];
            } else if (currentKey.equals("Resolutions")) {
                resInfoCount++;

                if (resInfoCount == 0) {
                    firstResolutions = new float[nDimensions];
                } else if (resInfoCount == 1) {
                    int numRes = 1;

                    if (nDimensions > 2) {
                        numRes *= fileInfo.getExtents()[2];

                        if (nDimensions > 3) {
                            numRes *= fileInfo.getExtents()[3];

                            if (nDimensions > 4) {
                                numRes *= fileInfo.getExtents()[4];
                            }
                        }
                    }

                    resolutions = new float[nDimensions][numRes];

                    for (int i = 0; i < nDimensions; i++) {
                        resolutions[i][0] = firstResolutions[i];
                    }

                }

                resolutionsCount = -1;

            } else if (currentKey.equals("Annotation")) {

                if (nDimensions > 2) {

                    annotationVOI = new VOI((short) annotationVector.size(), "annotation3d.voi",
                            VOI.ANNOTATION, -1.0f);
                } else {
                    annotationVOI = new VOI((short) annotationVector.size(), "annotation2d.voi", VOI.ANNOTATION,
                            -1.0f);
                }

            } else if (currentKey.equals("Talairach")) {

                // talairach = new TalairachTransformInfo();
                talairach.isAcpc(true);
                origAC = new float[3];
                origPC = new float[3];
                acpcPC = new float[3];
                origDim = new int[3];
                origOrigin = new float[3];
                origRes = new float[3];
                origOrientation = new float[9];
            } else if (currentKey.equals("tlrcInfo")) {
                talairach.isTlrc(true);
                acpcMin = new float[3];
                acpcMax = new float[3];
                tlrcRes = new float[7];
            }
            else if (currentKey.equals("VolumeGradients")) {
                gradients = new float[numVolumes][3];
                
            }
            else {
                elementBuffer = "";
            }
            

        }
    }
}
