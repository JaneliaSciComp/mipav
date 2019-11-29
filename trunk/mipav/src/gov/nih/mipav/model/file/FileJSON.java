package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelStorageBase;

import gov.nih.mipav.view.MipavUtil;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Vector;


/**
 
 */
public class FileJSON extends FileBase {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    int[] imgExtents;

    /** DOCUMENT ME! */
    int nDimensions = 2;

    /** DOCUMENT ME! */
    int sourceType = ModelStorageBase.FLOAT;

    /** DOCUMENT ME! */
    private boolean endianess;

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private FileInfoJSON fileInfo;

    private FileInfoJSON fileInfoCopy;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private ModelImage image[];

    /** DOCUMENT ME! */
    private ModelLUT LUT = null;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * JSON reader/writer constructor.
     * 
     * @param fileName file name
     * @param fileDir file directory
     * 
     * @exception IOException if there is an error making the file
     */
    public FileJSON(final String fileName, final String fileDir) throws IOException {

        this.fileName = fileName;
        this.fileDir = fileDir;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>.
     */
    public void finalize() {
        fileName = null;
        fileDir = null;
        fileInfo = null;
        fileInfoCopy = null;
        file = null;
        image = null;
        imgExtents = null;
        LUT = null;
        
        try {
            super.finalize();
        } catch (final Throwable er) {}
    }

    /**
     * returns LUT if defined.
     * 
     * @return the LUT if defined else it is null
     */
    public ModelLUT getModelLUT() {
        return LUT;
    }

    /**
     * reads the JSON file header and data.
     * 
     * @exception IOException if there is an error reading the file
     * 
     * @return DOCUMENT ME!
     */
    public ModelImage[] readImage() throws IOException {
        int i = 0;
        String s;
        boolean readAgain;
        boolean inInfoStructure = false;
        int index;
        int index2;
        int index3;
        int closingIndex;
        String description = null;
        String url = null;
        String version = null;
        String year = null;
        String contributor = null;
        String date_created = null;
        boolean inImagesStructure = false;
        boolean inIndividualImage = false;
        int numImages = 0;
        Vector<String>fileDirectoryVector = new Vector<String>();
        Vector<String>fileNameVector = new Vector<String>();
        Vector<Integer>heightVector = new Vector<Integer>();
        Vector<Integer>widthVector = new Vector<Integer>();
        String fDir;
        String fName;
        int height;
        int width;
        int extents[] = new int[2];
        boolean inAnnotationsStructure = false;
        boolean inObjectStructure = false;
        String xval;
        String yval;
        int xvalInt;
        int yvalInt;
        Vector<Integer>xvalVector = new Vector<Integer>();
        Vector<Integer>yvalVector = new Vector<Integer>();
        int image_id = 0;
        int id = 0;
        //int count = 0;
        int lastimage_id = -1;
        int buffer[] = null;
        boolean emptySegmentation = false;

        try {

            file = new File(fileDir + fileName);

            endianess = FileBase.BIG_ENDIAN; // true
            fileInfo = new FileInfoJSON(fileName, fileDir, FileUtility.JSON);
            fileInfo.setEndianess(endianess);

            raFile = new RandomAccessFile(file, "r");
            long fileLength = raFile.length();

            readAgain = true;

            while (readAgain) {
            	if (raFile.getFilePointer() >= (fileLength - 1)) {
                    break;
                }
                s = readLine();
                //count++;
                if ((s.startsWith("\"info\":"))  && (s.endsWith("{"))) {
                    inInfoStructure = true;
                    continue;
                }
                else if (inInfoStructure) {
	                if ((s.equals("}")) || (s.equals("},"))) {
	                	inInfoStructure = false;
	                	continue;
	                }
	                else if ((s.startsWith("\"description\":"))) {
	                	index  = s.indexOf(":");
	                	s = s.substring(index+1);
	                	index = s.indexOf("\"");
	                	index2 = s.lastIndexOf("\"");
	                	if ((index != -1) && (index2 > index)) {
	                		description = s.substring(index+1,index2);
	                	}
	                	continue;
	                } // else if ((s.startsWith("\"description\":")))
	                else if ((s.startsWith("\"url\":"))) {
	                	index  = s.indexOf(":");
	                	s = s.substring(index+1);
	                	index = s.indexOf("\"");
	                	index2 = s.lastIndexOf("\"");
	                	if ((index != -1) && (index2 > index)) {
	                		url = s.substring(index+1,index2);
	                	}
	                	continue;
	                } // else if ((s.startsWith("\"url\":")))
	                else if ((s.startsWith("\"version\":"))) {
	                	index  = s.indexOf(":");
	                	s = s.substring(index+1);
	                	index = s.indexOf("\"");
	                	index2 = s.lastIndexOf("\"version\":");
	                	if ((index != -1) && (index2 > index)) {
	                		version = s.substring(index+1,index2);
	                	}
	                	continue;
	                } // else if ((s.startsWith("\"version\":")))
	                else if ((s.startsWith("\"year\":"))) {
	                	index  = s.indexOf(":");
	                	s = s.substring(index+1);
	                	index = s.indexOf(",");
	                	if (index != -1) {
	                		year = s.substring(0,index).trim();
	                	}
	                	else {
	                		year = s.trim();
	                	}
	                	continue;
	                } // else if ((s.startsWith("\"year\":")))
	                else if ((s.startsWith("\"contributor\":"))) {
	                	index  = s.indexOf(":");
	                	s = s.substring(index+1);
	                	index = s.indexOf("\"");
	                	index2 = s.lastIndexOf("\"");
	                	if ((index != -1) && (index2 > index)) {
	                		contributor = s.substring(index+1,index2);
	                	}
	                	continue;
	                } // else if ((s.startsWith("\"contributor\":")))
	                else if ((s.startsWith("\"date_created\":"))) {
	                	index  = s.indexOf(":");
	                	s = s.substring(index+1);
	                	index = s.indexOf("\"");
	                	index2 = s.lastIndexOf("\"");
	                	if ((index != -1) && (index2 > index)) {
	                		date_created = s.substring(index+1,index2);
	                	}
	                	continue;
	                } // else if ((s.startsWith("\"date_created\":")))
                } // else if (inInfoStructure)
                else if ((s.startsWith("\"images\":"))  && (s.endsWith("["))) {
                    inImagesStructure = true;
                    continue;
                }
                else if (inImagesStructure) {
                    if (s.trim().equals("{")) {
                    	inIndividualImage = true;
                    	numImages++;
                    	continue;
                    }
                    else if (inIndividualImage && (s.trim().equals("}")) || (s.trim().equals("},"))) {
                    	inIndividualImage = false;
                    	continue;
                    }
                    else if (inIndividualImage) {
                        if (s.trim().startsWith("\"file_name\":"))	{
                        	index = s.indexOf(":");
                        	s = s.substring(index+1);
                        	index = s.indexOf("/");
    	                	index2 = s.lastIndexOf("/");
    	                	index3 = s.lastIndexOf(".");
    	                	fDir = s.substring(index,index2+1);
    	                	fName = s.substring(index2+1,index3);
    	                	fileDirectoryVector.add(fDir);
    	                	fileNameVector.add(fName);
    	                	continue;
                        }
                        else if (s.trim().startsWith("\"height\":")) {
                        	index = s.indexOf(":");
                        	s = s.substring(index+1).trim();
                        	try {
                        	    height = Integer.valueOf(s).intValue();
                        	}
                        	catch (NumberFormatException e) {
                        		MipavUtil.displayError("Height format exception on " + s);
                        		return null;
                        	}
                        	heightVector.add(height);
                        	continue;
                        }
                        else if (s.trim().startsWith("\"width\":")) {
                        	index = s.indexOf(":");
                        	s = s.substring(index+1).trim();
                        	try {
                        	    width = Integer.valueOf(s).intValue();
                        	}
                        	catch (NumberFormatException e) {
                        		MipavUtil.displayError("Width format exception on " + s);
                        		return null;
                        	}
                        	widthVector.add(width);
                        	continue;
                        }
                        else {
                        	continue;
                        }
                    } // else if (inIndividualImage)
                    else if ((!inIndividualImage) && ((s.trim().equals("]")) || (s.trim().equals("],")))) {
                    	inImagesStructure = false;
                    	image = new ModelImage[numImages];
                    	for (i = 0; i < numImages; i++) {
                    		extents[0] = widthVector.get(i);
                    		extents[1] = heightVector.get(i);
                    	    image[i] = new ModelImage(ModelStorageBase.USHORT, extents, fileNameVector.get(i));	
                    	    fileInfoCopy = new FileInfoJSON(fileNameVector.get(i),fileDir,FileUtility.JSON);
                    	    fileInfoCopy.setExtents(extents);
                    	    fileInfoCopy.setFileName(fileNameVector.get(i));
                    	    fileInfoCopy.setFileDirectory(fileDir);
                    	    fileInfoCopy.setDescription(description);
                    	    fileInfoCopy.setURL(url);
                    	    fileInfoCopy.setVersion(version);
                    	    fileInfoCopy.setYear(year);
                    	    fileInfoCopy.setContributor(contributor);
                    	    fileInfoCopy.setDate_created(date_created);
                    	    image[i].setFileInfo(fileInfoCopy,0);
                    	} // for (i = 0; i < numSlices; i++)
                    	continue;
                    } 
                } // else if (inImagesStructure)
                else if ((s.startsWith("\"annotations\":"))  && (s.endsWith("["))) {
                    inAnnotationsStructure = true;
                    continue;
                }
                else if (inAnnotationsStructure) {
                    if (s.trim().equals("{")) {
                    	inObjectStructure = true;
                    	continue;
                    }
                    else if (inObjectStructure && (s.trim().equals("}")) || (s.trim().equals("},"))) {
                    	if (emptySegmentation) {
                    		System.out.println("Empty segmentation in image_id = " + image_id + " id = " + id);
                    		continue;
                    	}
                    	System.out.println("image_id = " + image_id + " id = " + id);
                    	inObjectStructure = false;
                    	width = widthVector.get(image_id);
                    	height = heightVector.get(image_id);
                    	if ((image_id != lastimage_id) && (buffer != null)) {
                    		try {
                    			image[lastimage_id].importData(0, buffer, true);
                    		}
                    		catch (IOException e) {
                    			MipavUtil.displayError("IOException on image["+lastimage_id+"].importData(0,buffer,true)");
                    		}
                    	    buffer = new int[width*height];
                    	}
                    	else if ((image_id != lastimage_id) && (buffer == null)) {
                    		buffer = new int[width*height];
                    	}
                    	for (i = 0; i < xvalVector.size(); i++) {
                    		buffer[xvalVector.get(i) + width*yvalVector.get(i)] = (id + 1);
                    	}
                    	xvalVector.clear();
                    	yvalVector.clear();
                    	lastimage_id = image_id;
                    	continue;
                    }
                    else if (inObjectStructure) {
                    	if (s.trim().startsWith("\"segmentation\":"))	{
                    	    index = s.indexOf("[[");
                    	    closingIndex = s.indexOf("]]");
                    	    if (closingIndex == index+2) {
                    	        emptySegmentation = true;
                    	        continue;
                    	    }
                    	    emptySegmentation = false;
                    	    s = s.substring(index+2);
                    	    while (true) {
                    	    	index = s.indexOf(",");
                    	    	xval = s.substring(0,index).trim();
                    	    	try {
                    	    		xvalInt = Integer.valueOf(xval).intValue();
                    	    	}
                    	    	catch (NumberFormatException e) {
                    	    		MipavUtil.displayError("Format exception for xval on " + xval);
                    	    		return null;
                    	    	}
                    	    	xvalVector.add(xvalInt);
                    	    	s = s.substring(index+1);
                    	    	index = s.indexOf(",");
                    	    	closingIndex = s.indexOf("]]");
                    	    	if ((index != -1) && (index < closingIndex)) {
                    	    		yval = s.substring(0,index).trim();
                    	    		try {
                    	    			yvalInt = Integer.valueOf(yval).intValue();
                        	    	}
                        	    	catch (NumberFormatException e) {
                        	    		MipavUtil.displayError("Format exception for yval on " + yval);
                        	    		return null;
                        	    	}
                        	    	yvalVector.add(yvalInt);
                        	    	s = s.substring(index+1);
                    	    	}
                    	    	else {
                    	    		yval = s.substring(0,closingIndex).trim();
                    	    		try {
                    	    			yvalInt = Integer.valueOf(yval).intValue();
                        	    	}
                        	    	catch (NumberFormatException e) {
                        	    		MipavUtil.displayError("Format exception for yval on " + yval);
                        	    		return null;
                        	    	}
                        	    	yvalVector.add(yvalInt);
                        	    	break;
                    	    	}
                    	    }
                    	    continue;
                    	} // if (s.trim().startsWith("\"segmentation\":"))
                    	else if (s.trim().startsWith("\"image_id\":"))	{
                    		index = s.indexOf(":");
                    		s = s.substring(index+1).trim();
                    		try {
                    			image_id = Integer.valueOf(s).intValue();
                    		}
                    		catch (NumberFormatException e) {
                    			MipavUtil.displayError("Format error for image_id on " + s);
                    			return null;
                    		}
                    		continue;
                    	} // else if (s.trim().startsWith("\"image_id\":"))
                    	else if (s.trim().startsWith("\"id\":"))	{
                    		index = s.indexOf(":");
                    		s = s.substring(index+1).trim();
                    		try {
                    			id = Integer.valueOf(s).intValue();
                    		}
                    		catch (NumberFormatException e) {
                    			MipavUtil.displayError("Format error for id on " + s);
                    			return null;
                    		}
                    		continue;
                    	} // else if (s.trim().startsWith("\"id\":"))
                    } // else if (inObjectStructure)
                    else if ((!inObjectStructure) && ((s.trim().equals("]")) || (s.trim().equals("],")))) {
                    	System.out.println("Finishing image_id = " + image_id + " id = " + id);
                    	inAnnotationsStructure = false;
                    	width = widthVector.get(image_id);
                    	height = heightVector.get(image_id);
                    	for (i = 0; i < xvalVector.size(); i++) {
                    		buffer[xvalVector.get(i) + width*yvalVector.get(i)] = (id + 1);
                    	}
                    	xvalVector.clear();
                    	yvalVector.clear();
                    	try {
                			image[image_id].importData(0, buffer, true);
                		}
                		catch (IOException e) {
                			MipavUtil.displayError("IOException on image["+image_id+"].importData(0,buffer,true)");
                		}
                    	break;
                    }
                } // else if (inAnnotationsStructure)
                
            } // while(readAgain) looping for first required keyword of SIMPLE

           

            for (i = 0; i < image.length; i++) {
                image[i].calcMinMax();
            }
            raFile.close();

            return image;
        } catch (final Exception e) {

            if (image != null) {
            	for (i = 0; i < image.length; i++) {
                    image[i].disposeLocal();
                    image[i] = null;
            	}
            	image = null;
            }

            System.gc();
            throw new IOException();
        }
    }

   
    
    /**
     * readLine() - reads a line of the file and strips comments indicated by the # symbol.
     *
     * @return     the line read in
     *
     * @exception  IOException  if there is an error reading the file
     */
    private String readLine() throws IOException {
        String tempString;

        try {
            tempString = raFile.readLine();
        } catch (IOException error) {
            throw (error);
        }

        if (tempString == null){
            return null;
        }

      
        return tempString.trim();
    }

}
