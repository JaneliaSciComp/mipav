package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;

import javax.swing.tree.*;

import ncsa.hdf.object.*;
import ncsa.hdf.object.h5.*;
import ncsa.hdf.view.Tools;



public class FileMincHDF extends FileBase {

	//static final variables
	public static final String NODE_DIMENSIONS = "dimensions";
	public static final String NODE_IMAGE = "image";
	public static final String NODE_INFO = "info";
	public static final String LEAF_IMAGE = "image";
	public static final String LEAF_IMAGE_MAX = "image-max";
	public static final String LEAF_IMAGE_MIN = "image-min";
	public static final String LEAF_X_SPACE = "xspace";
	public static final String LEAF_Y_SPACE = "yspace";
	public static final String LEAF_Z_SPACE = "zspace";
	public static final String LEAF_ACQUISITION = "acquisition";
	public static final String LEAF_PATIENT = "patient";
	public static final String LEAF_PROCESSING = "processing";
	public static final String LEAF_STUDY = "study";

	public static final String ATTR_DIM_UNITS = "units";
	public static final String ATTR_DIM_START = "start";
	public static final String ATTR_DIM_LENGTH = "length";
	public static final String ATTR_DIM_DIRECTION_COSINES = "direction_cosines";
	public static final String ATTR_DIM_SPACETYPE = "spacetype";
	public static final String ATTR_DIM_STEP = "step";
	public static final String ATTR_DIM_ALIGNMENT = "alignment";

	public static final String ATTR_IMAGE_DIM_ORDER = "dimorder";
	public static final String ATTR_IMAGE_VALID_RANGE = "valid_range";
	
	public static final String ATTR_INFO_STUDY_MODALITY = "modality";
	
	
    //~ Instance fields ------------------------------------------------------------------------------------------------
  
    private DefaultMutableTreeNode imageNode;
    
    /**
     * The endianess of the image being written or read.
     *
     * <p>TODO: this variable either needs to be local to the read/write methods or consistently used globally. As it
     * stands right now, it is used in both ways and confuses things greatly.</p>
     */
    private boolean endianess;

    private double [] step = null;
    private double[][] dirCosines = null;
    private boolean [] isCentered = null;
    
    /** The directory containing the minc file being written out or read in. */
    private String fileDir;

    /** The name of the minc file to be read in or written out. */
    private String fileName;

    private FileFormat fileFormat = FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5);
    
    private H5File h5File;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * MINC reader/writer constructor.
     *
     * @param      fName  File name.
     * @param      fDir   File directory.
     *
     * @exception  IOException  if there is an error constructing the files
     */
    public FileMincHDF(String fName, String fDir) throws IOException {
        fileName = fName;
        fileDir = fDir;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

  
    public FileInfoMincHDF readHeader(DefaultMutableTreeNode rootNode) throws Exception {
       
    	FileInfoMincHDF fInfo = new FileInfoMincHDF(fileName, fileDir, FileUtility.MINC_HDF);
    
    	//for now just traverse the tree, see what's there
    	parseHDFHeader(rootNode, fInfo);

        return fInfo;
    }    
    
    private void parseHDFHeader(DefaultMutableTreeNode node, FileInfoMincHDF fInfo) throws Exception {
    	System.err.println("node: " + node + " , " + node.getClass());
  
    	if (node.toString().equals(NODE_DIMENSIONS)) {
    		parseDims(node, fInfo);
    		fInfo.setDimensionNode(node);
    		return;
    	} else if (node.toString().equals(NODE_INFO)) {
    		parseInfo(node, fInfo);
    		return;
    	}
    	
    	
    	
    	if (!node.isLeaf() && node.toString().equals(NODE_IMAGE)) {
    		imageNode = node;
    		return;
    	}
    	
    	long [] dataDims;
    	if (node.isLeaf()) {
    		HObject userObject = (HObject)node.getUserObject();
    	//	System.err.println("\t" + userObject.getClass());
    	//	System.err.println("\t" + userObject);
    		List metaData = userObject.getMetadata();
    		Iterator it = metaData.iterator();
    		while(it.hasNext()) {
    			Attribute currentAttribute = (Attribute)it.next();
    			dataDims = currentAttribute.getDataDims();
    		//	System.err.print("\t\tName: " + currentAttribute.getName());
    		//	System.err.print(", datatype(class): " + currentAttribute.getType().getDatatypeClass() + ", ");
    			for (int i = 0; i < dataDims.length; i++) {
    				//System.err.print(", " + dataDims[i]);
    				if (currentAttribute.getValue() instanceof String[]) {
    			//		System.err.print(", string val: " + ((String[])currentAttribute.getValue())[i]);
    				}
    			}
    			//System.err.println("");
    			//System.err.println(currentAttribute.getValue());
    		}
    		
    	}
    	
    	//recursively calls parseHDFHeader to reach every node/leaf
    	for (int i = 0; i < node.getChildCount(); i++) {
    		parseHDFHeader((DefaultMutableTreeNode)node.getChildAt(i), fInfo);
    	}
    }
    
    /**
     * Determines the dimensions, dirCosines, isCentered, step, and alignment
     * @param dimensionNode
     * @param fInfo
     * @throws Exception
     */
    private void parseDims(DefaultMutableTreeNode dimensionNode, FileInfoMincHDF fInfo) throws Exception {    	
    	DefaultMutableTreeNode currentDimNode;
    	int currentDim = 0;
    	int totalDims = dimensionNode.getChildCount();
    	int [] extents = new int[totalDims];
    	int [] units = new int[totalDims];
    	double [] startLoc = new double[totalDims];
    	step = new double[totalDims];
    	isCentered = new boolean[totalDims];
    	
    	dirCosines = new double[totalDims][totalDims];
    	
    	for (int i = 0; i < dimensionNode.getChildCount(); i++) {
    		currentDimNode = (DefaultMutableTreeNode)dimensionNode.getChildAt(i);
    		System.err.println("userobject: " + currentDimNode.getUserObject());
    		if (currentDimNode.getUserObject().toString().equals(LEAF_X_SPACE)) {
    			currentDim = 0;
    		} else if (currentDimNode.getUserObject().toString().equals(LEAF_Y_SPACE)) {
    			currentDim = 1;
    		} else if (currentDimNode.getUserObject().toString().equals(LEAF_Z_SPACE)) {
    			currentDim = 2;
    		}
    		
    		List metaData = ((HObject)currentDimNode.getUserObject()).getMetadata();
    		Iterator it = metaData.iterator();
    		while(it.hasNext()) {
    			Attribute attr = (Attribute)it.next();
    			String attrName = attr.getName();
    			if (attrName.equals(ATTR_DIM_UNITS)) {
    				units[currentDim] = FileInfoBase.getUnitsOfMeasureFromStr(((String[])attr.getValue())[0]);
    			} else if (attrName.equals(ATTR_DIM_START)) {
    				startLoc[currentDim] = ((double[])attr.getValue())[0];
    			} else if (attrName.equals(ATTR_DIM_LENGTH)) {
    				extents[currentDim] = ((int[])attr.getValue())[0];
    			} else if (attrName.equals(ATTR_DIM_DIRECTION_COSINES)) {
    				dirCosines[currentDim] = (double[])attr.getValue();
    			} else if (attrName.equals(ATTR_DIM_SPACETYPE)) {
    				
    			} else if (attrName.equals(ATTR_DIM_STEP)) {
    				step[currentDim] = ((double[])attr.getValue())[0];
    			} else if (attrName.equals(ATTR_DIM_ALIGNMENT)) {
    				isCentered[currentDim] = ((String[])attr.getValue())[0].equals("centre");
    			}
    			
    		}
    		    		
    	}
    	
    	float [] res = new float[totalDims];
    	for (int i = 0; i < totalDims; i++) {
    		res[i] = Math.abs((float)step[i]);
    	}
    	
    	fInfo.setResolutions(res);
    	fInfo.setExtents(extents);
    	fInfo.setUnitsOfMeasure(units);
    	
    	TransMatrix axisMat = new TransMatrix(4);
    	axisMat.setMatrix(dirCosines);
    	fInfo.setAxisOrientation(FileInfoMinc.getAxisOrientation(axisMat));
    	
    	
    	//for (int i = 0; i < totalDims; i++) {
    	//	System.err.println("extents[" + i + "]: " + extents[i]);
    	//	System.err.println("units[" + i + "]: " + units[i]);
    	//	System.err.println("startLoc[" + i + "]: " + startLoc[i]);
    	//	for (int j = 0; j < totalDims; j++) {
    	//		System.err.println("dirCosines[" + i + "][" + j + "]: " + dirCosines[i][j]);
    	//	}
		//}
    }
    
    /**
     * Parses the info node (right now only grabs the modality)
     * @param infoNode
     * @param fInfo
     * @throws Exception
     */
    private void parseInfo(DefaultMutableTreeNode infoNode, FileInfoMincHDF fInfo) throws Exception {
    	DefaultMutableTreeNode currentInfoNode;
    	fInfo.setInfoNode(infoNode);
    	for (int i = 0; i < infoNode.getChildCount(); i++) {
    		currentInfoNode = (DefaultMutableTreeNode)infoNode.getChildAt(i);
    		if (currentInfoNode.getUserObject().toString().equals(LEAF_STUDY)) {
    			Iterator it = ((HObject)currentInfoNode.getUserObject()).getMetadata().iterator();
    			while(it.hasNext()) {
    				Attribute attr = (Attribute)it.next();
    				String attrName = attr.getName();
        			if (attrName.equals(ATTR_INFO_STUDY_MODALITY)) {
        				String modality = ((String[])attr.getValue())[0];
        				fInfo.setModality(modality);
        			}
    			}
    		}
    		
    		
    	}
    }
    
    /**
     * Parses the image node and reads in the image data (and image min and max per slice)
     * @param iNode
     * @param fInfo
     * @return
     * @throws Exception
     */
    private ModelImage parseImage(DefaultMutableTreeNode iNode, FileInfoMincHDF fInfo) throws Exception {
    	
    	if (iNode.getChildCount() > 1) {
    		MipavUtil.displayError("Does not support MINC 2.0 with more than one child below \"image\" node");
    		return null;
    	}
    	
    	ModelImage image = null;
    	DefaultMutableTreeNode childNode = (DefaultMutableTreeNode)iNode.getChildAt(0);
    	DefaultMutableTreeNode currentNode;
    	double[] imageMax = null;
    	double[] imageMin = null;
    	int numImages = 1;
    	
    	for (int i = 0; i < childNode.getChildCount(); i++) {
    		currentNode = (DefaultMutableTreeNode)childNode.getChildAt(i);
    		if (currentNode.getUserObject().toString().equals(LEAF_IMAGE)) {
    			//parse the image data here
    			
    			//determine the Datatype class (int)
    			int dataType = ((H5ScalarDS)currentNode.getUserObject()).getDatatype().getDatatypeClass();
    			int sign = ((H5ScalarDS)currentNode.getUserObject()).getDatatype().getDatatypeSign();
    			boolean isUnsigned = ((H5ScalarDS)currentNode.getUserObject()).getDatatype().isUnsigned();
    			int order = ((H5ScalarDS)currentNode.getUserObject()).getDatatype().getDatatypeOrder();
    			int size = ((H5ScalarDS)currentNode.getUserObject()).getDatatype().getDatatypeSize();
    			
    			int mDataType = 0;
    			switch (dataType) {
    				case Datatype.CLASS_FLOAT:	
    					mDataType = ModelStorageBase.DOUBLE;
    					break;
    				case Datatype.CLASS_INTEGER:
    					if (size == 1 && isUnsigned) {
    						mDataType = ModelStorageBase.USHORT;
    					} else if (size == 2 && !isUnsigned) {
    						mDataType = ModelStorageBase.SHORT;
    					} else if (size == 2 && isUnsigned) {
    						mDataType = ModelStorageBase.UINTEGER;
    					} else if (size == 4 && !isUnsigned) {
    						mDataType = ModelStorageBase.INTEGER;
    					}
    					break;
    			}
    			//set the MIPAV data type
    			fInfo.setDataType(mDataType);
    			
    			//set the endianess
    			fInfo.setEndianess(order == Datatype.ORDER_BE);    			
    			
    			//System.err.println("type: " + dataType + ", size: " + size + ", sign: " + sign + ", order: " + order + ", mipav type: " + mDataType);
    			
    			List imageMetaData = ((HObject)currentNode.getUserObject()).getMetadata();
        		Iterator it = imageMetaData.iterator();
    			while(it.hasNext()) {
    				Attribute imageAttr = (Attribute)it.next();
    				String attrName = imageAttr.getName();
    				if (attrName.equals(ATTR_IMAGE_DIM_ORDER)) {
    					String dimOrder = ((String[])imageAttr.getValue())[0];
    					//System.err.println("Dim order string: " + dimOrder);
    					
    					if (dimOrder.startsWith("zspace")) {
    			            fInfo.setImageOrientation(FileInfoBase.AXIAL);
    			        } else if (dimOrder.startsWith("xspace")) {
    			        	fInfo.setImageOrientation(FileInfoBase.SAGITTAL);
    			        } else if (dimOrder.startsWith("yspace")) {
    			        	fInfo.setImageOrientation(FileInfoBase.CORONAL);
    			        }
    					StringTokenizer tokens = new StringTokenizer(dimOrder, ",");
    					String[] dimStrings = new String[fInfo.getExtents().length];
    					int dimCount = 0;
    					while(tokens.hasMoreTokens()) {
    						dimStrings[dimCount] = tokens.nextToken();
    						dimCount++;
    					}
    					int [] axisOrientation = new int[fInfo.getExtents().length];
    					
    					if (axisOrientation.length == 3) {
    						axisOrientation[0] = FileInfoMinc.setOrientType(dimStrings[2], (step[2] > 0));
    						axisOrientation[1] = FileInfoMinc.setOrientType(dimStrings[1], (step[1] > 0));
    						axisOrientation[2] = FileInfoMinc.setOrientType(dimStrings[0], (step[0] > 0));
    					}
    					fInfo.setAxisOrientation(axisOrientation);
    					
    				} else if (attrName.equals(ATTR_IMAGE_VALID_RANGE)) {
    					double [] validRange = ((double[])imageAttr.getValue());
    					fInfo.setValidRange(validRange);
    					System.err.println("Valid range: " + validRange[0] + " to " + validRange[1]);
    				}
    				
    			}
        		
    			H5ScalarDS imageData = (H5ScalarDS)currentNode.getUserObject();
    			int rank = imageData.getRank();
    	        if (rank <=0) {
    	        	imageData.init();
    	        }
    	        
    	        boolean is3D = (imageData.getRank() > 2) && !((ScalarDS)imageData).isTrueColor();
    	        
    	        int w = imageData.getWidth();
    	        int h = imageData.getHeight();
    	        System.err.println("dataset width: " + w + ", height: " + h);
    			
    			//System.err.println("datarange: " + dataRange[0] + " to " + dataRange[1]);
    			image = new ModelImage(fInfo.getDataType(), fInfo.getExtents(), fileName);
    			
    			
    			if (is3D) {
    				numImages = fInfo.getExtents()[2];
    			}
    			
    			Object data = null;
    			
    	       
    	        long[] start = imageData.getStartDims(); // the starting dims
    	         	        
    	        
    	        int sliceSize = w * h;
    			for (int j = 0; j < numImages; j++) {
    				start[0] = j;
    				//System.err.println(imageData.getStartDims()[0]);
    				data = imageData.read();
    				
    				if (data instanceof short[]) {
    					image.importData(j * sliceSize, (short[])data, true);
    				} else if (data instanceof int[]) {
    					image.importData(j * sliceSize, (int[])data, true);
    				} else if (data instanceof byte[]) {
    					image.importData(j * sliceSize, (byte[])data, true);
    				} else if (data instanceof float[]) {
    					image.importData(j * sliceSize, (float[])data, true);
    				} else if (data instanceof double[]) {
    					image.importData(j * sliceSize, (double[])data, true);
    				} 
    				fireProgressStateChanged(Math.round(5 + ((float)j/numImages) * 95 ));
    			}
    			
    			
    		} else if (currentNode.getUserObject().toString().equals(LEAF_IMAGE_MAX)) {
    			H5ScalarDS data = (H5ScalarDS)currentNode.getUserObject();
    			data.init();
    			imageMax = (double[])data.getData();
    		} else if (currentNode.getUserObject().toString().equals(LEAF_IMAGE_MIN)) {
    			H5ScalarDS data = (H5ScalarDS)currentNode.getUserObject();
    			data.init();
    			imageMin = (double[])data.getData();
    			
    		}
    				
    	}
    	
    	FileInfoMincHDF [] fileInfos = new FileInfoMincHDF[numImages];
    	fileInfos[0] = fInfo;
    	
    	double [] rescaleSlope = null;
    	double [] rescaleIntercept = null;
    	//if imageMax and imageMin were present, do the rescaleintercept here
    	if (imageMax != null && imageMin != null && fInfo.getValidRange() != null) {
    		
    		rescaleSlope = new double[numImages];
    		rescaleIntercept = new double[numImages];
    		
    		FileInfoMincHDF.calculateRescaleIntercept(rescaleIntercept, rescaleSlope, imageMax, imageMin, fInfo.getValidRange());
    		fileInfos[0].setRescaleIntercept(rescaleIntercept[0]);
    		fileInfos[0].setRescaleSlope(rescaleSlope[0]);
    		fileInfos[0].setValidRange(fInfo.getValidRange());
    	}
    	
    	
    	for (int i = 1; i < numImages; i++) {
    		fileInfos[i] = (FileInfoMincHDF)fInfo.clone();
    		fileInfos[i].setValidRange(fInfo.getValidRange());
    		//set rescaleslope and intercept on fInfos
    		if (rescaleSlope != null & rescaleIntercept != null) {
    			fileInfos[i].setRescaleIntercept(rescaleIntercept[i]);
    			fileInfos[i].setRescaleSlope(rescaleSlope[i]);    			
    		}
    		
    		//set start locations (convert to dicom)
    		if (step != null && dirCosines != null && isCentered != null) {
    			fileInfos[i].setStartLocations(fileInfos[i].getConvertStartLocationsToDICOM(step, dirCosines, isCentered, i));
    		}
    		
    	}
    	image.setFileInfo(fileInfos);
    	
    	
    	
    	return image;
    }
    
    /**
     * Reads a MINC image file and stores the data in file info.
     *
     * @param      one  Flag indicating if only one image should be read in
     *
     * @return     The image.
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @see        FileRaw
     */
    public ModelImage readImage(boolean one) throws Exception {
    	ModelImage image = null;
    	DefaultMutableTreeNode fileRoot = null;
    	try {
    		h5File = (H5File)fileFormat.createInstance(fileDir + fileName, FileFormat.READ);
    		int fid = h5File.open();
    		System.err.println("file-id: " + fid);
    	} catch (Exception e) {
    		return null;
    	}
    	
    	fileRoot = (DefaultMutableTreeNode)h5File.getRootNode();
    	System.err.println("file root: " + fileRoot);
    	System.err.println("file root type: " + fileRoot.getClass());
    	FileInfoMincHDF fInfo = readHeader(fileRoot);
    	fireProgressStateChanged(5);
    	if (imageNode != null) {
    		image = parseImage(imageNode, fInfo);
    	}
    	
        return image;
    }

    private void buildHDF(ModelImage image, FileWriteOptions options) throws Exception {
    	FileInfoBase fInfo = image.getFileInfo()[0];
    	String fullPath = fileDir + File.separator + "test-" + fileName;
    	
    	//create the h5File and open it
    	H5File h5File = new H5File(fullPath,
    			H5File.WRITE);
    	
    	FileFormat format = h5File.createFile(fullPath, H5File.FILE_CREATE_DELETE);
    	int fid = h5File.open();
    	
    	// create the fileRoot (H5File)
    	DefaultMutableTreeNode fileRoot = (DefaultMutableTreeNode)h5File.getRootNode();
    	DefaultTreeModel model = new DefaultTreeModel(fileRoot);

    	//create the minc 2.0 group
    	Group mincGroup = format.createGroup("minc-2.0", (Group)fileRoot.getUserObject());
    	    	
    	HDFNode mincNode = new HDFNode(mincGroup);
        model.insertNodeInto(mincNode, fileRoot, fileRoot.getChildCount());
    	
        
        //if image was MINC_HDF, the first slice's fileinfo contains the dimension and info nodes
        if (fInfo instanceof FileInfoMincHDF) {
        	DefaultMutableTreeNode dimNode = ((FileInfoMincHDF)fInfo).getDimensionNode();
        	format.copy((HObject)dimNode.getUserObject(), mincGroup, "dimensions");
        	
        	model.insertNodeInto(dimNode, mincNode, mincNode.getChildCount());
        	
        	DefaultMutableTreeNode infoNode = ((FileInfoMincHDF)fInfo).getInfoNode();
        	format.copy((HObject)infoNode.getUserObject(), mincGroup, "info");
        	model.insertNodeInto(infoNode, mincNode, mincNode.getChildCount());
        
        } else {
        	//image was not MINC_HDF to begin with, so must create the dimension node
        	
        	HDFNode dimNode = buildDimensionNode(image, mincNode);
        	format.copy((HObject)dimNode.getUserObject(), mincGroup, "dimensions");
        	model.insertNodeInto(dimNode, mincNode, mincNode.getChildCount());
        	
        	HDFNode infoNode = buildInfoNode(image, mincNode);
        	format.copy((HObject)infoNode.getUserObject(), mincGroup, "info");
        	model.insertNodeInto(infoNode, mincNode, mincNode.getChildCount());
        }
    	
        buildImageNode(image, format, mincNode, model);
        
        
    	h5File.close();
    }
    
    private HDFNode buildDimensionNode(ModelImage image, HDFNode mincNode) {
    	HDFNode dimNode = null;
    	
    	
    	return dimNode;
    }
    
    private HDFNode buildInfoNode(ModelImage image, HDFNode mincNode) {
    	HDFNode infoNode = null;
    	
    	
    	return infoNode;
    }
    
    private void buildImageNode(ModelImage image, FileFormat format, HDFNode mincNode, DefaultTreeModel model) throws Exception {
    	
    	//create the image node (root of image)
    	HDFNode imageNode = null;
    	Group imageGroup = format.createGroup("image", (Group)mincNode.getUserObject());
    
    	    	imageNode = new HDFNode(imageGroup);
    	model.insertNodeInto(imageNode, mincNode, 1);
    	
    	//create the image number node (only 1, named "0")
    	Group imageNumGroup = format.createGroup("0", (Group)imageNode.getUserObject());
    	HDFNode imageNumNode = new HDFNode(imageNumGroup);
    	model.insertNodeInto(imageNumNode, imageNode, imageNode.getChildCount());
    	
    	//create the actual image data node (leaf)
    	
    	//determine the class type
    	int mDataType = image.getType();
    	int tclass = 0;
    	int tsign = 0;
    	int tsize = 0;
    	switch (mDataType) {
    		case ModelStorageBase.USHORT:
    			tclass = Datatype.CLASS_INTEGER;
    			tsize = 1 << 1;
    			tsign = Datatype.SIGN_NONE;
    			break;
    		case ModelStorageBase.SHORT:
    			tclass = Datatype.CLASS_INTEGER;
    			tsize = 1 << 1;
    			tsign = Datatype.SIGN_2;
    			break;
    		case ModelStorageBase.UINTEGER:
    			tclass = Datatype.CLASS_INTEGER;
    			tsize = 1 << 2;
    			tsign = Datatype.SIGN_NONE;
    			break;
    		case ModelStorageBase.INTEGER:
    			tclass = Datatype.CLASS_INTEGER;
    			tsize = 1 << 2;
    			tsign = Datatype.SIGN_2;
    			break;
    		default:
    			System.err.println("not valid yet...");
    			return;
    	}
    	
    	int torder = 0;
    	if (image.getFileInfo()[0].getEndianess()) {
    		torder = Datatype.ORDER_BE;
    	} else {
    		torder = Datatype.ORDER_LE;
    	}
    	
    	System.err.println("tclass: " + tclass + ", tsize: " + tsize + ", torder: " + torder + ", tsign: " + tsign);
    	Datatype datatype = fileFormat.createDatatype(tclass, tsize, torder, tsign);
        System.err.println("datatype: " + datatype);
    	
    	long [] dims = new long[image.getExtents().length];
    	long [] maxdims = new long[dims.length];
    	if (dims.length == 2) {
    		
    	} else {
    		//order is z, y, x
    		dims[0] = image.getExtents()[2];
    		dims[1] = image.getExtents()[1];
    		dims[2] = image.getExtents()[0];
    		
    		maxdims[0] = dims[0];
    		maxdims[1] = dims[1];
    		maxdims[2] = dims[2];
    	}
    	long [] chunks = null;
    	int gzip = 0;
    	
    	//create the Dataset (H5ScalarDS) that will hold the image data
        H5ScalarDS imageObj = (H5ScalarDS)fileFormat.createScalarDS("image", imageNumGroup, datatype,
            dims, maxdims, chunks, gzip, null);
        DefaultMutableTreeNode imageDataNode = new DefaultMutableTreeNode(imageObj);
        model.insertNodeInto(imageDataNode, imageNumNode, imageNumNode.getChildCount());
        
        imageObj.init();
        System.err.println("H5ScalarDS width: " + imageObj.getWidth() + ", height: " + imageObj.getHeight());
        
        
        //import the data 
        long[] start = imageObj.getStartDims(); // the starting dims
	
	    
        int sliceSize = image.getSliceSize();
        int numImages = 1;
        if (image.getExtents().length > 2) {
        	numImages = image.getExtents()[2];
        }
        
        Object dataImportObj = null;
        switch (mDataType) {
        case ModelStorageBase.USHORT:
        case ModelStorageBase.SHORT:
        	dataImportObj = new short[sliceSize];
			break;
		case ModelStorageBase.UINTEGER:
		case ModelStorageBase.INTEGER:
			dataImportObj = new int[sliceSize];
			break;
		default:
			System.err.println("not valid yet...");
			return;
        }
        
		for (int j = 0; j < numImages; j++) {
			start[0] = j;
			//System.err.println(imageData.getStartDims()[0]);
			//data = imageData.read();
			switch (mDataType) {
	        case ModelStorageBase.USHORT:
	        case ModelStorageBase.SHORT:
	        	image.exportData(j * sliceSize, sliceSize, (short[])dataImportObj);
				break;
			case ModelStorageBase.UINTEGER:
			case ModelStorageBase.INTEGER:
				image.exportData(j * sliceSize, sliceSize, (int[])dataImportObj);
				break;
			default:
				System.err.println("not valid yet...");
				return;
	        }
			imageObj.write(dataImportObj);
		}
		
		//write attributes to the Image data node
		
        tclass = Datatype.CLASS_FLOAT;
        tsize = 4;
        torder = Datatype.NATIVE;
		Datatype validRangeType = fileFormat.createDatatype(tclass, tsize, torder, Datatype.SIGN_2);
		long[] attrDims = { 2 };
		Attribute attr = new Attribute("valid_range", validRangeType, attrDims);
		if (image.getFileInfo()[0] instanceof FileInfoMincHDF) {
			attr.setValue(((FileInfoMincHDF)image.getFileInfo()[0]).getValidRange());
		} else {
			//not supported yet
		}
		
		System.err.println("valid range: " + ((FileInfoMincHDF)image.getFileInfo()[0]).getValidRange()[0] + " to "
				+ ((FileInfoMincHDF)image.getFileInfo()[0]).getValidRange()[1]);
        imageObj.writeMetadata(attr);
		
		
		//create the image-max and image-min arrays (always 64bit float?)
		tsize = 8;
		Datatype imageRangeDatatype = fileFormat.createDatatype(Datatype.CLASS_FLOAT, tsize, -1, Datatype.SIGN_2);
		dims = new long[1];
		maxdims = new long[1];
		dims[0] = 1;
		if (image.getExtents().length > 2) {
			dims[0] = image.getExtents()[2];
		}
		maxdims[0] = dims[0];
		
		H5ScalarDS imageMaxObj = (H5ScalarDS)fileFormat.createScalarDS("image-max", imageNumGroup, imageRangeDatatype,
	            dims, maxdims, chunks, gzip, null);
		DefaultMutableTreeNode imageMaxNode = new DefaultMutableTreeNode(imageMaxObj);
        model.insertNodeInto(imageMaxNode, imageNumNode, imageNumNode.getChildCount());
        
        H5ScalarDS imageMinObj = (H5ScalarDS)fileFormat.createScalarDS("image-min", imageNumGroup, imageRangeDatatype,
	            dims, maxdims, chunks, gzip, null);
		DefaultMutableTreeNode imageMinNode = new DefaultMutableTreeNode(imageMinObj);
        model.insertNodeInto(imageMinNode, imageNumNode, imageNumNode.getChildCount());
		
        double [] imageMax = new double[(int)dims[0]];
        double [] imageMin = new double[(int)dims[0]];
        
        double rescaleIntercept = 0;
        double rescaleSlope = 0;
        
        double [] imageSliceBuffer = new double[image.getSliceSize()];
        
        for (int i = 0; i < dims[0]; i++) {
        	image.exportData(i * sliceSize, sliceSize, imageSliceBuffer);
        	rescaleSlope = image.getFileInfo()[i].getRescaleSlope();
        	rescaleIntercept = image.getFileInfo()[i].getRescaleIntercept();
        	
        	
        	imageMax[i] = imageSliceBuffer[0];
        	imageMin[i] = imageSliceBuffer[0];
        	for (int j = 0; j < imageSliceBuffer.length; j++) {
        		if (imageSliceBuffer[j] > imageMax[i]) {
        			imageMax[i] = imageSliceBuffer[j];
        		} else if (imageSliceBuffer[j] < imageMin[i]) {
        			imageMin[i] = imageSliceBuffer[j];
        		}
        	}
        	imageMax[i] = (imageMax[i] * rescaleSlope) + rescaleIntercept;
        	imageMin[i] = (imageMin[i] * rescaleSlope) + rescaleIntercept;
        	
        }
        //init and then read in the two arrays
        imageMaxObj.init();
        imageMinObj.init();
        
        imageMaxObj.write(imageMax);
        imageMinObj.write(imageMin);
        
    }
    
    /**
     * Writes a MINC format type image. Calls the appropriate header method.
     *
     * @param      image   Image model where the data is stored.
     * @param      options  Information about how to write this file.
     *
     * @exception  IOException  if there is an error writing the file
     *
     * @see        FileInfoMinc
     * @see        FileMinc
     */
    public void writeImage(ModelImage image, FileWriteOptions options) throws Exception {
    	
    	buildHDF(image, options);
    }

    public class HDFNode extends DefaultMutableTreeNode {
    	public static final long serialVersionUID = HObject.serialVersionUID;
    	public HDFNode(Group group) {
    		super(group);
    	}
    	public boolean isLeaf() { return false; }
    }
    
}
