package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

import java.io.*;
import java.util.*;

import javax.swing.tree.*;

import ncsa.hdf.object.*;
import ncsa.hdf.object.h5.*;


/**
 * HDF5 based reader/writer for MINC 2.0
 * 
 * @author linkb
 */
public class FileMincHDF extends FileBase {
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

    public static final String ATTR_INFO_SLICE_THICKNESS = "slice_thickness";

    public static String DICOM_GROUP_PREFIX = new String("dicom_0x");

    public static String DICOM_ELEMENT_PREFIX = new String("el_0x");

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** the image node of the HDF file tree that contains the image data, and min max per slice information */
    private DefaultMutableTreeNode imageNode;

    private String[] dimStringsReordered = null;

    private String[] dimStrings = null;

    /** MINC resolution (can be negative) */
    private double[] step = null;

    private double[] stepReordered = null;

    /** direction cosines matrix */
    private double[][] dirCosines = null;

    /** whether each axis is centered */
    private boolean[] isCentered = null;

    private double[] mincStartLoc = null;

    /** The directory containing the minc file being written out or read in. */
    private String fileDir;

    /** The name of the minc file to be read in or written out. */
    private String fileName;

    /** The ordering of the dimensions. Should be found in the image node attributes. */
    private String[] dimOrder;

    /** will always use the HDF5 fileformat */
    private final FileFormat fileFormat = FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5);

    // private FileFormat fileFormat;

    /** the file to be read/written */
    private H5File h5File;

    private FileInfoMincHDF fileInfo;

    private int[] dimReorderIndexes = null;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * MINC reader/writer constructor.
     * 
     * @param fName File name.
     * @param fDir File directory.
     * 
     * @exception IOException if there is an error constructing the files
     */
    public FileMincHDF(final String fName, final String fDir) throws IOException {
        fileName = fName;
        fileDir = fDir;
        fileInfo = new FileInfoMincHDF(fileName, fileDir, FileUtility.MINC_HDF);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>.
     */
    public void finalize() {
        int i;
        fileName = null;
        fileDir = null;
        fileInfo = null;
        if (imageNode != null) {
            imageNode.removeAllChildren();
            imageNode = null;
        }
        step = null;
        if (dirCosines != null) {
            for (i = 0; i < dirCosines.length; i++) {
                dirCosines[i] = null;
            }
            dirCosines = null;
        }
        isCentered = null;
        mincStartLoc = null;
        if (dimOrder != null) {
            for (i = 0; i < dimOrder.length; i++) {
                dimOrder[i] = null;
            }
            dimOrder = null;
        }

        try {
            super.finalize();
        } catch (final Throwable er) {}
    }

    public void readHeader(final DefaultMutableTreeNode rootNode) throws Exception {
        // before recursively parsing the tree, get the image dim order from the image var attributes
        parseImageDimOrder(rootNode);

        // for now just traverse the tree, see what's there
        parseHDFHeader(rootNode);
    }

    /**
     * Returns the FileInfoXML read from the file.
     * 
     * @return File info read from file, or null if it has not been read.
     */
    public FileInfoMincHDF getFileInfo() {
        return fileInfo;
    }

    /**
     * recursively parses the HDF tree into the given file info
     * 
     * @param node
     * @param fileInfo
     * @throws Exception
     */
    private void parseHDFHeader(final DefaultMutableTreeNode node) throws Exception {
        // System.err.println("node: " + node + " , " + node.getClass());

        // look for the dimension and info nodes, pass them off to functions for parsing
        if (node.toString().equals(FileMincHDF.NODE_DIMENSIONS)) {
            try {
                parseDims(node);
            } catch (final Exception e) {
                e.printStackTrace();
            }
            fileInfo.setDimensionNode(node);
            return;
        } else if (node.toString().equals(FileMincHDF.NODE_INFO)) {
            parseInfo(node);
            return;
        }

        // set the image node when found (will parse later)
        if ( !node.isLeaf() && node.toString().equals(FileMincHDF.NODE_IMAGE)) {
            imageNode = node;
            return;
        }

        if ( !node.isLeaf()) {
            // recursively calls parseHDFHeader to reach every non-leaf
            for (int i = 0; i < node.getChildCount(); i++) {
                parseHDFHeader((DefaultMutableTreeNode) node.getChildAt(i));
            }
        }
    }

    /**
     * Recursively finds the node within a tree with a given name.
     * 
     * @param nodeToSearch the root node of the tree to search
     * @param nodeNameToFind the name of the node to find
     * @return a node with the name given, or null if no node was found within the given tree
     */
    private static final DefaultMutableTreeNode findNode(final DefaultMutableTreeNode nodeToSearch,
            final String nodeNameToFind) {
        DefaultMutableTreeNode childResult;

        if (nodeToSearch.toString().equals(nodeNameToFind)) {
            return nodeToSearch;
        } else {
            for (int i = 0; i < nodeToSearch.getChildCount(); i++) {
                childResult = FileMincHDF.findNode((DefaultMutableTreeNode) nodeToSearch.getChildAt(i), nodeNameToFind);
                if (childResult != null) {
                    return childResult;
                }
            }
        }

        // no children, or all children also returned null
        return null;
    }

    /**
     * 
     * @param rootNode
     */
    private void parseImageDimOrder(final DefaultMutableTreeNode rootNode) throws Exception {
        // find image/0/image node (not just image since image/0/image-min and image/0/image-max also have dimorder
        // attributes)
        final DefaultMutableTreeNode imageNode = FileMincHDF.findNode((DefaultMutableTreeNode) FileMincHDF.findNode(
                rootNode, FileMincHDF.NODE_IMAGE).getChildAt(0), FileMincHDF.LEAF_IMAGE);

        List<Attribute> imageMetaData;
        if (imageNode == null) {
            System.out.println("imageNode == null");
        }
        else if (imageNode.getUserObject() == null) {
            System.out.println("imageNode.getUserObject() == null");
        }
        imageMetaData = ((HObject) imageNode.getUserObject()).getMetadata();

        final Iterator<Attribute> it = imageMetaData.iterator();
        while (it.hasNext()) {
            final Attribute imageAttr = it.next();
            if (imageAttr.getName().equals(FileMincHDF.ATTR_IMAGE_DIM_ORDER)) {
                final String dimOrderString = ((String[]) imageAttr.getValue())[0];

                // System.err.println("Dim order string parsehdfheader: " + dimOrderString);

                if (dimOrderString.startsWith("zspace")) {
                    fileInfo.setImageOrientation(FileInfoBase.AXIAL);
                } else if (dimOrderString.startsWith("xspace")) {
                    fileInfo.setImageOrientation(FileInfoBase.SAGITTAL);
                } else if (dimOrderString.startsWith("yspace")) {
                    fileInfo.setImageOrientation(FileInfoBase.CORONAL);
                }

                final StringTokenizer tokens = new StringTokenizer(dimOrderString, ",");
                dimOrder = new String[tokens.countTokens()];
                int curDim = 0;
                while (tokens.hasMoreTokens()) {
                    dimOrder[curDim] = tokens.nextToken();
                    curDim++;
                }

                final int[] axisOrientation = new int[fileInfo.getExtents().length];

                if (axisOrientation.length == 3) {
                    axisOrientation[0] = FileInfoMinc.setOrientType(dimOrder[2], (step[2] > 0));
                    axisOrientation[1] = FileInfoMinc.setOrientType(dimOrder[1], (step[1] > 0));
                    axisOrientation[2] = FileInfoMinc.setOrientType(dimOrder[0], (step[0] > 0));
                }
                fileInfo.setAxisOrientation(axisOrientation);
            }
        }
    }

    /**
     * Determines the dimensions, dirCosines, isCentered, step, and alignment
     * 
     * @param dimensionNode
     * @param fileInfo
     * @throws Exception
     */
    private void parseDims(final DefaultMutableTreeNode dimensionNode) throws Exception {
        DefaultMutableTreeNode currentDimNode;
        final int totalDims = dimensionNode.getChildCount();
        final int[] extents = new int[totalDims];
        final int[] units = new int[totalDims];
        mincStartLoc = new double[totalDims];
        step = new double[totalDims];
        stepReordered = new double[totalDims];
        isCentered = new boolean[totalDims];
        dirCosines = new double[totalDims][totalDims];
        dimStringsReordered = new String[totalDims];
        dimStrings = new String[totalDims];

        dimReorderIndexes = new int[dimOrder.length];

        for (int i = 0; i < dimOrder.length; i++) {
            if (dimOrder[i].equals(FileMincHDF.LEAF_X_SPACE)) {
                dimReorderIndexes[i] = 2;

            } else if (dimOrder[i].equals(FileMincHDF.LEAF_Y_SPACE)) {
                dimReorderIndexes[i] = 1;

            } else if (dimOrder[i].equals(FileMincHDF.LEAF_Z_SPACE)) {
                dimReorderIndexes[i] = 0;

            }
        }

        // following is needed to determine the order of the axes
        // this is needed when setting up the axes orientations in mipav
        // which uses the dimString and the step
        int order = -1;
        for (int i = 0; i < dimensionNode.getChildCount(); i++) {
            currentDimNode = (DefaultMutableTreeNode) dimensionNode.getChildAt(i);
            if (currentDimNode.toString().equals(FileMincHDF.LEAF_X_SPACE)) {
                for (int k = 0; k < dimReorderIndexes.length; k++) {
                    if (dimReorderIndexes[k] == 2) {
                        order = k;
                        break;
                    }
                }

            } else if (currentDimNode.toString().equals(FileMincHDF.LEAF_Y_SPACE)) {
                for (int k = 0; k < dimReorderIndexes.length; k++) {
                    if (dimReorderIndexes[k] == 1) {
                        order = k;
                        break;
                    }
                }

            } else if (currentDimNode.toString().equals(FileMincHDF.LEAF_Z_SPACE)) {
                for (int k = 0; k < dimReorderIndexes.length; k++) {
                    if (dimReorderIndexes[k] == 0) {
                        order = k;
                        break;
                    }
                }
            }

            dimStringsReordered[order] = currentDimNode.toString();
            dimStrings[dimReorderIndexes[i]] = currentDimNode.toString();
            final List<Attribute> metaData = ((HObject) currentDimNode.getUserObject()).getMetadata();
            final Iterator<Attribute> it = metaData.iterator();
            while (it.hasNext()) {
                final Attribute attr = it.next();
                final String attrName = attr.getName();

                if (attrName.equals(FileMincHDF.ATTR_DIM_UNITS)) {
                    units[dimReorderIndexes[i]] = FileInfoBase
                            .getUnitsOfMeasureFromStr( ((String[]) attr.getValue())[0]);
                } else if (attrName.equals(FileMincHDF.ATTR_DIM_START)) {
                    mincStartLoc[dimReorderIndexes[i]] = ((double[]) attr.getValue())[0];
                } else if (attrName.equals(FileMincHDF.ATTR_DIM_LENGTH)) {
                    extents[dimReorderIndexes[i]] = ((int[]) attr.getValue())[0];
                } else if (attrName.equals(FileMincHDF.ATTR_DIM_DIRECTION_COSINES)) {
                    dirCosines[dimReorderIndexes[i]] = (double[]) attr.getValue();
                } else if (attrName.equals(FileMincHDF.ATTR_DIM_SPACETYPE)) {

                } else if (attrName.equals(FileMincHDF.ATTR_DIM_STEP)) {

                    step[dimReorderIndexes[i]] = ((double[]) attr.getValue())[0];
                    stepReordered[order] = ((double[]) attr.getValue())[0];
                } else if (attrName.equals(FileMincHDF.ATTR_DIM_ALIGNMENT)) {
                    isCentered[dimReorderIndexes[i]] = ((String[]) attr.getValue())[0].equals("centre");
                }
            }
        }

        // determine the resolutions (from the step)
        final float[] res = new float[totalDims];
        for (int i = 0; i < totalDims; i++) {
            res[i] = Math.abs((float) step[i]);
        }

        fileInfo.setResolutions(res);
        fileInfo.setExtents(extents);
        fileInfo.setUnitsOfMeasure(units);

        final TransMatrix axisMat = new TransMatrix(3);
        axisMat.copyMatrix(dirCosines);

        fileInfo.setAxisOrientation(FileInfoMinc.getAxisOrientation(axisMat));
    }

    /**
     * Parses the info node (right now only grabs the modality)
     * 
     * @param infoNode
     * @param fileInfo
     * @throws Exception
     */
    private void parseInfo(final DefaultMutableTreeNode infoNode) throws Exception {
        DefaultMutableTreeNode currentInfoNode;
        fileInfo.setInfoNode(infoNode);

        for (int i = 0; i < infoNode.getChildCount(); i++) {
            currentInfoNode = (DefaultMutableTreeNode) infoNode.getChildAt(i);
            if (currentInfoNode.getUserObject().toString().equals(FileMincHDF.LEAF_STUDY)) {
                final Iterator<Attribute> it = ((HObject) currentInfoNode.getUserObject()).getMetadata().iterator();
                while (it.hasNext()) {
                    final Attribute attr = it.next();
                    final String attrName = attr.getName();
                    if (attrName.equals(FileMincHDF.ATTR_INFO_STUDY_MODALITY)) {
                        final String modality = ((String[]) attr.getValue())[0];
                        fileInfo.setModality(modality);
                    }
                }
            } else if (currentInfoNode.getUserObject().toString().equals(FileMincHDF.LEAF_ACQUISITION)) {
                final Iterator<Attribute> it = ((HObject) currentInfoNode.getUserObject()).getMetadata().iterator();
                while (it.hasNext()) {
                    final Attribute attr = it.next();
                    final String attrName = attr.getName();
                    if (attrName.equals(FileMincHDF.ATTR_INFO_SLICE_THICKNESS)) {
                        final double thickness = ((double[]) attr.getValue())[0];
                        fileInfo.setSliceThickness((float) thickness);
                    }
                }
            } else if (currentInfoNode.getUserObject().toString().startsWith(FileMincHDF.DICOM_GROUP_PREFIX)) {
                String dicomGroup = currentInfoNode.getUserObject().toString();
                dicomGroup = dicomGroup.substring(FileMincHDF.DICOM_GROUP_PREFIX.length());

                final Iterator<Attribute> it = ((HObject) currentInfoNode.getUserObject()).getMetadata().iterator();
                while (it.hasNext()) {
                    final Attribute attr = it.next();
                    String dicomElement = attr.getName();

                    if (dicomElement.startsWith(FileMincHDF.DICOM_ELEMENT_PREFIX)) {
                        dicomElement = dicomElement.substring(FileMincHDF.DICOM_ELEMENT_PREFIX.length());
                        try {
                            final FileDicomKey key = new FileDicomKey(Integer.parseInt(dicomGroup, 16), Integer
                                    .parseInt(dicomElement, 16));
                            FileDicomTagInfo info = DicomDictionary.getInfo(key);

                            if (info == null) {
                                info = new FileDicomTagInfo(key, "UT", 1, "Private Tag", "Private Tag");
                            }

                            final FileDicomTag tag = new FileDicomTag(info);

                            if (attr.getValue() instanceof byte[]) {
                                tag.setValue(attr.getValue());
                            } else {
                                tag.setValue( ((Object[]) attr.getValue())[0]);
                            }

                            fileInfo.getDicomTable().put(key, tag);
                        } catch (final Exception e) {
                            e.printStackTrace();
                            System.err.println(dicomGroup + "\t" + dicomElement + "\t"
                                    + ((Object[]) attr.getValue())[0]);
                        }
                    }
                }
            }
        }
    }

    /**
     * Parses the image node and reads in the image data (and image min and max per slice)
     * 
     * @param iNode
     * @param fileInfo
     * @return
     * @throws Exception
     */
    private ModelImage parseImage(final DefaultMutableTreeNode iNode) throws Exception {
        if (iNode.getChildCount() > 1) {
            MipavUtil.displayError("Does not support MINC 2.0 with more than one child below \"image\" node");
            return null;
        }

        ModelImage image = null;
        final DefaultMutableTreeNode childNode = (DefaultMutableTreeNode) iNode.getChildAt(0);
        DefaultMutableTreeNode currentNode;
        double[] imageMax = null;
        double[] imageMin = null;
        int numImages = 1;

        fileInfo.setImageNode(childNode);

        for (int i = 0; i < childNode.getChildCount(); i++) {
            currentNode = (DefaultMutableTreeNode) childNode.getChildAt(i);
            if (currentNode.getUserObject().toString().equals(FileMincHDF.LEAF_IMAGE)) {
                // parse the image data here

                // determine the Datatype class (int)
                final int dataType = ((H5ScalarDS) currentNode.getUserObject()).getDatatype().getDatatypeClass();
                final int sign = ((H5ScalarDS) currentNode.getUserObject()).getDatatype().getDatatypeSign();
                final boolean isUnsigned = ((H5ScalarDS) currentNode.getUserObject()).getDatatype().isUnsigned();
                final int order = ((H5ScalarDS) currentNode.getUserObject()).getDatatype().getDatatypeOrder();
                final int size = ((H5ScalarDS) currentNode.getUserObject()).getDatatype().getDatatypeSize();

                int mDataType = 0;
                switch (dataType) {
                    case Datatype.CLASS_FLOAT:
                        if (size == 4) {
                            mDataType = ModelStorageBase.FLOAT;
                        } else {
                            mDataType = ModelStorageBase.DOUBLE;
                        }
                        break;
                    case Datatype.CLASS_INTEGER:
                        if (size == 1) {
                            if (isUnsigned) {
                                mDataType = ModelStorageBase.UBYTE;
                            } else {
                                mDataType = ModelStorageBase.BYTE;
                            }
                        } else if (size == 2) {
                            if (isUnsigned) {
                                mDataType = ModelStorageBase.USHORT;
                            } else {
                                mDataType = ModelStorageBase.SHORT;
                            }
                        } else if (size == 4) {
                            if (isUnsigned) {
                                mDataType = ModelStorageBase.UINTEGER;
                            } else {
                                mDataType = ModelStorageBase.INTEGER;
                            }
                        }
                        break;
                    case Datatype.CLASS_CHAR:
                        // some CLASS_CHAR datasets were showing up as CLASS_INTEGER with HDF-java 2.4
                        if (size == 1) {
                            if (isUnsigned) {
                                mDataType = ModelStorageBase.UBYTE;
                            } else {
                                mDataType = ModelStorageBase.BYTE;
                            }
                        } else if (size == 2) {
                            if (isUnsigned) {
                                mDataType = ModelStorageBase.USHORT;
                            } else {
                                mDataType = ModelStorageBase.SHORT;
                            }
                        } else if (size == 4) {
                            if (isUnsigned) {
                                mDataType = ModelStorageBase.UINTEGER;
                            } else {
                                mDataType = ModelStorageBase.INTEGER;
                            }
                        }
                        break;
                    default:
                        throw new IOException("Minc2-HDF: Unsupported image data type: " + dataType);
                }

                // set the MIPAV data type
                fileInfo.setDataType(mDataType);

                // set the endianess
                fileInfo.setEndianess(order == Datatype.ORDER_BE);

                // System.err.println("type: " + dataType + ", size: " + size + ", sign: " + sign + ", order: " + order
                // + ", mipav type: " + mDataType);

                final List<Attribute> imageMetaData = ((HObject) currentNode.getUserObject()).getMetadata();
                final Iterator<Attribute> it = imageMetaData.iterator();
                while (it.hasNext()) {
                    final Attribute imageAttr = it.next();
                    final String attrName = imageAttr.getName();
                    if (attrName.equals(FileMincHDF.ATTR_IMAGE_DIM_ORDER)) {
                        final String dimOrder = ((String[]) imageAttr.getValue())[0];
                        if (dimOrder.startsWith("zspace")) {
                            fileInfo.setImageOrientation(FileInfoBase.AXIAL);
                        } else if (dimOrder.startsWith("xspace")) {
                            fileInfo.setImageOrientation(FileInfoBase.SAGITTAL);
                        } else if (dimOrder.startsWith("yspace")) {
                            fileInfo.setImageOrientation(FileInfoBase.CORONAL);
                        }

                        final int[] axisOrientation = new int[fileInfo.getExtents().length];

                        if (axisOrientation.length == 3) {

                            axisOrientation[0] = FileInfoMinc.setOrientType(dimStringsReordered[2],
                                    (stepReordered[2] > 0));
                            axisOrientation[1] = FileInfoMinc.setOrientType(dimStringsReordered[1],
                                    (stepReordered[1] > 0));
                            axisOrientation[2] = FileInfoMinc.setOrientType(dimStringsReordered[0],
                                    (stepReordered[0] > 0));
                        }
                        fileInfo.setAxisOrientation(axisOrientation);

                    } else if (attrName.equals(FileMincHDF.ATTR_IMAGE_VALID_RANGE)) {
                        final double[] validRange = ((double[]) imageAttr.getValue());
                        fileInfo.setValidRange(validRange);
                    }

                }

                final H5ScalarDS imageData = (H5ScalarDS) currentNode.getUserObject();
                final int rank = imageData.getRank();
                if (rank <= 0) {
                    imageData.init();
                }

                final boolean is3D = (imageData.getRank() > 2) && ! ((ScalarDS) imageData).isTrueColor();

                final int w = imageData.getWidth();
                final int h = imageData.getHeight();
                // System.err.println("dataset width: " + w + ", height: " + h);

                // System.err.println("datarange: " + dataRange[0] + " to " + dataRange[1]);
                image = new ModelImage(fileInfo.getDataType(), fileInfo.getExtents(), fileName);

                if (is3D) {
                    numImages = fileInfo.getExtents()[2];
                }

                Object data = null;

                final long[] start = imageData.getStartDims(); // the starting dims

                final int sliceSize = w * h;
                for (int j = 0; j < numImages; j++) {
                    start[0] = j;
                    data = imageData.read();

                    if (fileInfo.getDataType() == ModelStorageBase.SHORT
                            || fileInfo.getDataType() == ModelStorageBase.USHORT) {
                        image.importData(j * sliceSize, (short[]) data, true);
                    } else if (fileInfo.getDataType() == ModelStorageBase.INTEGER
                            || fileInfo.getDataType() == ModelStorageBase.UINTEGER) {
                        image.importData(j * sliceSize, (int[]) data, true);
                    } else if (fileInfo.getDataType() == ModelStorageBase.BYTE
                            || fileInfo.getDataType() == ModelStorageBase.UBYTE) {
                        image.importData(j * sliceSize, (byte[]) data, true);
                    } else if (fileInfo.getDataType() == ModelStorageBase.FLOAT) {
                        image.importData(j * sliceSize, (float[]) data, true);
                    } else if (fileInfo.getDataType() == ModelStorageBase.DOUBLE) {
                        image.importData(j * sliceSize, (double[]) data, true);
                    }

                    fireProgressStateChanged(Math.round(5 + ((float) j / numImages) * 95));
                }

            } else if (currentNode.getUserObject().toString().equals(FileMincHDF.LEAF_IMAGE_MAX)) {
                final H5ScalarDS data = (H5ScalarDS) currentNode.getUserObject();
                data.init();
                imageMax = (double[]) data.getData();
            } else if (currentNode.getUserObject().toString().equals(FileMincHDF.LEAF_IMAGE_MIN)) {
                final H5ScalarDS data = (H5ScalarDS) currentNode.getUserObject();
                data.init();
                imageMin = (double[]) data.getData();

            }

        }

        // create the array of fileinfos for the model image
        final FileInfoMincHDF[] fileInfos = new FileInfoMincHDF[numImages];
        fileInfos[0] = fileInfo;

        double[] rescaleSlope = null;
        double[] rescaleIntercept = null;
        // if imageMax and imageMin were present, do the rescaleintercept here
        if (imageMax != null && imageMin != null && fileInfo.getValidRange() != null) {

            rescaleSlope = new double[numImages];
            rescaleIntercept = new double[numImages];

            FileInfoMincHDF.calculateRescaleIntercept(rescaleIntercept, rescaleSlope, imageMax, imageMin, fileInfo
                    .getValidRange());
            fileInfos[0].setRescaleIntercept(rescaleIntercept[0]);
            fileInfos[0].setRescaleSlope(rescaleSlope[0]);
            fileInfos[0].setValidRange(fileInfo.getValidRange());
        }

        // set the dicom tag hashtable in the first file info
        if (fileInfo.getDicomTable() != null) {
            fileInfos[0].setDicomTable(fileInfo.getDicomTable());
        }

        // set the origin on the first slice
        if (step != null && dirCosines != null && isCentered != null) {
            fileInfos[0].setStartLocations(fileInfos[0].getConvertStartLocationsToDICOM(step, dirCosines, isCentered,
                    0, mincStartLoc));
        }

        for (int i = 1; i < numImages; i++) {
            fileInfos[i] = (FileInfoMincHDF) fileInfo.clone();
            fileInfos[i].setValidRange(fileInfo.getValidRange());
            // set rescaleslope and intercept on fInfos
            if (rescaleSlope != null & rescaleIntercept != null) {
                fileInfos[i].setRescaleIntercept(rescaleIntercept[i]);
                fileInfos[i].setRescaleSlope(rescaleSlope[i]);
            }

            // set start locations (convert to dicom)
            if (step != null && dirCosines != null && isCentered != null) {
                fileInfos[i].setStartLocations(fileInfos[i].getConvertStartLocationsToDICOM(step, dirCosines,
                        isCentered, i, mincStartLoc));
            }

        }
        image.setFileInfo(fileInfos);

        return image;
    }

    /**
     * Reads a MINC 2.0 HDF-5 file
     * 
     * @param one
     * @return the image
     * @throws Exception
     */
    public ModelImage readImage(final boolean one) throws Exception {
        ModelImage image = null;
        DefaultMutableTreeNode fileRoot = null;
        /*
         * try { Class fileclass = Class.forName("gov.nih.mipav.model.file.HDF.H5File"); fileFormat =
         * (FileFormat)fileclass.newInstance(); if (fileFormat != null) { FileFormat.addFileFormat("HDF5", fileFormat); } }
         * catch (Throwable err ) {;}
         */
        // fileFormat = FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5);
        try {
            h5File = (H5File) fileFormat.createInstance(fileDir + fileName, FileFormat.READ);
            // h5File = new H5File(fileDir + fileName, FileFormat.READ);
            final int fid = h5File.open();
        } catch (final Exception e) {
            e.printStackTrace();
            return null;
        }

        fileRoot = (DefaultMutableTreeNode) h5File.getRootNode();
        readHeader(fileRoot);
        fireProgressStateChanged(5);
        if (imageNode != null) {
            image = parseImage(imageNode);
        }

        return image;
    }

    /**
     * Creates the dimension node for files that are not Minc2.0
     * 
     * @param image
     * @param format
     * @param mincNode
     * @param model
     * @return
     * @throws Exception
     */
    private void buildDimensionNode(final ModelImage image, final FileWriteOptions options, final FileFormat format,
            final HDFNode mincNode, final DefaultTreeModel model) throws Exception {
        HDFNode dimNode = null;
        final Group dimGroup = format.createGroup("dimensions", (Group) mincNode.getUserObject());

        dimNode = new HDFNode(dimGroup);
        model.insertNodeInto(dimNode, mincNode, mincNode.getChildCount());

        // build the xspace, yspace and zspace nodes
        final Datatype datatype = fileFormat.createDatatype(Datatype.CLASS_INTEGER, 4, Datatype.ORDER_NONE,
                Datatype.SIGN_2);
        final long[] dims = new long[] {1};
        final long[] maxdims = new long[] {1};

        // create the Strings[] used
        final String[] varIDString = new String[] {"MINC standard variable"};
        final String[] varTypeString = new String[] {"dimension____"};
        final String[] versionString = new String[] {"MINC Version    1.0"};
        // TODO: not sure that these comments are accurate all of the time, but they seem to be in every minc2 this way
        final String[] xcommentsString = new String[] {"X increases from patient left to right"};
        final String[] ycommentsString = new String[] {"Y increases from patient posterior to anterior"};
        final String[] zcommentsString = new String[] {"Z increases from patient inferior to superior"};
        final String[] spacingString = new String[] {"regular__"};
        final String[] spaceTypeString = new String[] {"native____"};
        final String[] alignmentString = new String[] {"centre"};
        String[] unitsString;
        double[][] scannerArray = null;

        final H5ScalarDS xSpaceObj = (H5ScalarDS) fileFormat.createScalarDS("xspace", dimGroup, datatype, dims,
                maxdims, null, 0, null);
        final DefaultMutableTreeNode xSpaceNode = new DefaultMutableTreeNode(xSpaceObj);
        model.insertNodeInto(xSpaceNode, dimNode, dimNode.getChildCount());
        
        if ((image.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL)) 
            ||  (image.getFileInfo()[0].getFileFormat() == FileUtility.DICOM)) {
            scannerArray = new double[4][4];
            (image.getMatrix()).copyMatrix(scannerArray);
            // MINC is L->R and P->A while MIPAV is R->L and A->P, so multiply first 2 rows by -1
            for (int i = 0; i <= 1; i++) {
                for (int j = 0; j < 3; j ++) {
                    scannerArray[i][j] *= -1;
                }
            }
            // Normalize each column length to be unit length
            for (int j = 0; j < 3; j++) {
                double length = 0.0;
                for (int i = 0; i < 3; i++) {
                    length += scannerArray[i][j] * scannerArray[i][j];
                }
                length = Math.sqrt(length);
                for (int i = 0; i < 3; i++) {
                    scannerArray[i][j] /= length;
                }
            }
        }

        // build xspace, yspace and zspace starts and steps

        // System.err.println("xstart: " + options.getXStart() + ", xstep: " + options.getXSpace());
        // System.err.println("ystart: " + options.getYStart() + ", ystep: " + options.getYSpace());
        // System.err.println("zstart: " + options.getZStart() + ", zstep: " + options.getZSpace());

        double xstart, ystart, zstart;
        double xstep, ystep, zstep;

        switch (options.getAxisOrientation()[2]) {

            case FileInfoBase.ORI_L2R_TYPE:
            case FileInfoBase.ORI_R2L_TYPE:
                zstart = options.getXStart();
                zstep = options.getXSpace();
                break;

            case FileInfoBase.ORI_A2P_TYPE:
            case FileInfoBase.ORI_P2A_TYPE:
                zstart = options.getYStart();
                zstep = options.getYSpace();
                break;

            case FileInfoBase.ORI_I2S_TYPE:
            case FileInfoBase.ORI_S2I_TYPE:
            default:
                zstart = options.getZStart();
                zstep = options.getZSpace();
        }

        switch (options.getAxisOrientation()[1]) {

            case FileInfoBase.ORI_L2R_TYPE:
            case FileInfoBase.ORI_R2L_TYPE:
                ystart = options.getXStart();
                ystep = options.getXSpace();
                break;

            case FileInfoBase.ORI_A2P_TYPE:
            case FileInfoBase.ORI_P2A_TYPE:
                ystart = options.getYStart();
                ystep = options.getYSpace();
                break;

            case FileInfoBase.ORI_I2S_TYPE:
            case FileInfoBase.ORI_S2I_TYPE:
                ystart = options.getZStart();
                ystep = options.getZSpace();
                break;

            default:
                ystart = options.getYStart();
                ystep = options.getYSpace();
        }

        switch (options.getAxisOrientation()[0]) {

            case FileInfoBase.ORI_L2R_TYPE:
            case FileInfoBase.ORI_R2L_TYPE:
                xstart = options.getXStart();
                xstep = options.getXSpace();
                break;

            case FileInfoBase.ORI_A2P_TYPE:
            case FileInfoBase.ORI_P2A_TYPE:
                xstart = options.getYStart();
                xstep = options.getYSpace();
                break;

            case FileInfoBase.ORI_I2S_TYPE:
            case FileInfoBase.ORI_S2I_TYPE:
                xstart = options.getZStart();
                xstep = options.getZSpace();
                break;

            default:
                xstart = options.getXStart();
                xstep = options.getXSpace();
        }

        // varid

        Datatype dType = fileFormat.createDatatype(Datatype.CLASS_STRING, varIDString[0].length() + 1, Datatype.NATIVE,
                -1);
        final long[] attrDims = {1};
        final Attribute varIDAttr = new Attribute("varid", dType, attrDims);
        varIDAttr.setValue(varIDString);
        xSpaceObj.writeMetadata(varIDAttr);

        // vartype

        dType = fileFormat.createDatatype(Datatype.CLASS_STRING, varIDString[0].length() + 1, Datatype.NATIVE, -1);
        attrDims[0] = 1;
        final Attribute varTypeAttr = new Attribute("vartype", dType, attrDims);
        varTypeAttr.setValue(varTypeString);
        xSpaceObj.writeMetadata(varTypeAttr);

        // version

        dType = fileFormat.createDatatype(Datatype.CLASS_STRING, versionString[0].length() + 1, Datatype.NATIVE, -1);
        final Attribute versionAttr = new Attribute("version", dType, attrDims);
        versionAttr.setValue(versionString);
        xSpaceObj.writeMetadata(versionAttr);

        // comments

        dType = fileFormat.createDatatype(Datatype.CLASS_STRING, xcommentsString[0].length() + 1, -1, -1);
        final Attribute commentsAttr = new Attribute("comments", dType, attrDims);
        commentsAttr.setValue(xcommentsString);
        xSpaceObj.writeMetadata(commentsAttr);

        // spacing

        dType = fileFormat.createDatatype(Datatype.CLASS_STRING, spacingString[0].length() + 1, -1, -1);
        final Attribute spacingAttr = new Attribute("spacing", dType, attrDims);
        spacingAttr.setValue(spacingString);
        xSpaceObj.writeMetadata(spacingAttr);

        // alignment

        dType = fileFormat.createDatatype(Datatype.CLASS_STRING, alignmentString[0].length() + 1, -1, -1);
        final Attribute alignmentAttr = new Attribute("alignment", dType, attrDims);
        alignmentAttr.setValue(alignmentString);
        xSpaceObj.writeMetadata(alignmentAttr);

        // step (resolution) - will be taken from the dialog's xSpace var
        dType = fileFormat.createDatatype(Datatype.CLASS_FLOAT, 8, Datatype.NATIVE, Datatype.SIGN_NONE);
        Attribute stepAttr = new Attribute("step", dType, attrDims);
        stepAttr.setValue(new double[] {xstep});
        xSpaceObj.writeMetadata(stepAttr);

        // start (origin)
        dType = fileFormat.createDatatype(Datatype.CLASS_FLOAT, 8, Datatype.NATIVE, Datatype.SIGN_NONE);
        final Attribute startAttr = new Attribute("start", dType, attrDims);
        startAttr.setValue(new double[] {xstart});
        xSpaceObj.writeMetadata(startAttr);

        // direction_cosines
        dType = fileFormat.createDatatype(Datatype.CLASS_FLOAT, 8, Datatype.NATIVE, Datatype.SIGN_NONE);
        attrDims[0] = 3;
        Attribute dirCosinesAttr = new Attribute("direction_cosines", dType, attrDims);
        // create an array here
        double[] dirCosines;
        if (scannerArray != null) {
            dirCosines = new double[] {scannerArray[0][0], scannerArray[1][0], scannerArray[2][0]};    
        }
        else {
            dirCosines = new double[] {1, 0, 0};
        }
        dirCosinesAttr.setValue(dirCosines);
        xSpaceObj.writeMetadata(dirCosinesAttr);

        // units
        unitsString = new String[] {FileInfoBase.getUnitsOfMeasureAbbrevStr()[image.getFileInfo()[0]
                .getUnitsOfMeasure(0)]};
        dType = fileFormat.createDatatype(Datatype.CLASS_STRING, alignmentString[0].length() + 1, -1, -1);
        attrDims[0] = 1;
        Attribute unitsAttr = new Attribute("units", dType, attrDims);
        unitsAttr.setValue(unitsString);
        xSpaceObj.writeMetadata(unitsAttr);

        // spacetype
        dType = fileFormat.createDatatype(Datatype.CLASS_STRING, spaceTypeString[0].length() + 1, -1, -1);
        final Attribute spaceTypeAttr = new Attribute("spacetype", dType, attrDims);
        spaceTypeAttr.setValue(spaceTypeString);
        xSpaceObj.writeMetadata(spaceTypeAttr);

        // length
        int[] length = new int[] {image.getExtents()[0]};
        dType = fileFormat.createDatatype(Datatype.CLASS_INTEGER, 4, Datatype.NATIVE, Datatype.SIGN_NONE);
        final Attribute lengthAttr = new Attribute("length", dType, attrDims);
        lengthAttr.setValue(length);
        xSpaceObj.writeMetadata(lengthAttr);

        // YSPACE NODE
        final H5ScalarDS ySpaceObj = (H5ScalarDS) fileFormat.createScalarDS("yspace", dimGroup, datatype, dims,
                maxdims, null, 0, null);
        final DefaultMutableTreeNode ySpaceNode = new DefaultMutableTreeNode(ySpaceObj);
        model.insertNodeInto(ySpaceNode, dimNode, dimNode.getChildCount());

        // varid
        ySpaceObj.writeMetadata(varIDAttr);

        // vartype
        ySpaceObj.writeMetadata(varTypeAttr);

        // version
        ySpaceObj.writeMetadata(versionAttr);

        // comments
        commentsAttr.setValue(ycommentsString);
        ySpaceObj.writeMetadata(commentsAttr);

        // spacing
        ySpaceObj.writeMetadata(spacingAttr);

        // alignment
        ySpaceObj.writeMetadata(alignmentAttr);

        // step (resolution)
        dType = fileFormat.createDatatype(Datatype.CLASS_FLOAT, 8, Datatype.NATIVE, Datatype.SIGN_NONE);
        stepAttr = new Attribute("step", dType, attrDims);
        stepAttr.setValue(new double[] {ystep});
        ySpaceObj.writeMetadata(stepAttr);

        // start (origin)
        startAttr.setValue(new double[] {ystart});
        ySpaceObj.writeMetadata(startAttr);

        // direction_cosines
        if (scannerArray != null) {
            dirCosines = new double[] {scannerArray[0][1], scannerArray[1][1], scannerArray[2][1]};       
        }
        else {
            dirCosines = new double[] {0, 1, 0};
        }
        dType = fileFormat.createDatatype(Datatype.CLASS_FLOAT, 8, Datatype.NATIVE, Datatype.SIGN_NONE);
        attrDims[0] = 3;
        dirCosinesAttr = new Attribute("direction_cosines", dType, attrDims);
        dirCosinesAttr.setValue(dirCosines);
        ySpaceObj.writeMetadata(dirCosinesAttr);

        // units
        attrDims[0] = 1;
        unitsString = new String[] {FileInfoBase.getUnitsOfMeasureAbbrevStr()[image.getFileInfo()[0]
                .getUnitsOfMeasure(1)]};
        dType = fileFormat.createDatatype(Datatype.CLASS_STRING, alignmentString[0].length() + 1, -1, -1);
        unitsAttr = new Attribute("units", dType, attrDims);
        unitsAttr.setValue(unitsString);
        ySpaceObj.writeMetadata(unitsAttr);

        // spacetype
        ySpaceObj.writeMetadata(spaceTypeAttr);

        // length
        length = new int[] {image.getExtents()[1]};
        lengthAttr.setValue(length);
        ySpaceObj.writeMetadata(lengthAttr);

        if (image.getExtents().length > 2) {
            // YSPACE NODE
            final H5ScalarDS zSpaceObj = (H5ScalarDS) fileFormat.createScalarDS("zspace", dimGroup, datatype, dims,
                    maxdims, null, 0, null);
            final DefaultMutableTreeNode zSpaceNode = new DefaultMutableTreeNode(zSpaceObj);
            model.insertNodeInto(zSpaceNode, dimNode, dimNode.getChildCount());

            // varid
            zSpaceObj.writeMetadata(varIDAttr);

            // vartype
            zSpaceObj.writeMetadata(varTypeAttr);

            // version
            zSpaceObj.writeMetadata(versionAttr);

            // comments
            commentsAttr.setValue(zcommentsString);
            zSpaceObj.writeMetadata(commentsAttr);

            // spacing
            zSpaceObj.writeMetadata(spacingAttr);

            // alignment
            zSpaceObj.writeMetadata(alignmentAttr);

            // step (resolution)
            dType = fileFormat.createDatatype(Datatype.CLASS_FLOAT, 8, Datatype.NATIVE, Datatype.SIGN_NONE);
            stepAttr = new Attribute("step", dType, attrDims);
            stepAttr.setValue(new double[] {zstep});
            zSpaceObj.writeMetadata(stepAttr);

            // start (origin)
            startAttr.setValue(new double[] {zstart});
            zSpaceObj.writeMetadata(startAttr);

            // direction_cosines
            if (scannerArray != null) {
                dirCosines = new double[] {scannerArray[0][2], scannerArray[1][2], scannerArray[2][2]};  
            }
            else {
                dirCosines = new double[] {0, 0, 1};
            }
            dType = fileFormat.createDatatype(Datatype.CLASS_FLOAT, 8, Datatype.NATIVE, Datatype.SIGN_NONE);
            attrDims[0] = 3;
            dirCosinesAttr = new Attribute("direction_cosines", dType, attrDims);
            dirCosinesAttr.setValue(dirCosines);
            zSpaceObj.writeMetadata(dirCosinesAttr);

            // units
            attrDims[0] = 1;
            unitsString = new String[] {FileInfoBase.getUnitsOfMeasureAbbrevStr()[image.getFileInfo()[0]
                    .getUnitsOfMeasure(1)]};
            dType = fileFormat.createDatatype(Datatype.CLASS_STRING, alignmentString[0].length() + 1, -1, -1);
            unitsAttr = new Attribute("units", dType, attrDims);
            unitsAttr.setValue(unitsString);
            zSpaceObj.writeMetadata(unitsAttr);

            // spacetype
            zSpaceObj.writeMetadata(spaceTypeAttr);

            // length
            length = new int[] {image.getExtents()[2]};
            lengthAttr.setValue(length);
            zSpaceObj.writeMetadata(lengthAttr);
        }

    }

    /**
     * builds the Info node for files that were not originally MINC 2.0 HDF5
     * 
     * @param image
     * @param format
     * @param mincNode
     * @param model
     * @throws Exception
     */
    private void buildInfoNode(final ModelImage image, final FileFormat format, final HDFNode mincNode,
            final DefaultTreeModel model) throws Exception {
        if (fileInfo.getDicomTable() == null) {
            return;
        }

        HDFNode infoNode = null;
        final Group infoGroup = format.createGroup("info", (Group) mincNode.getUserObject());

        infoNode = new HDFNode(infoGroup);
        model.insertNodeInto(infoNode, mincNode, mincNode.getChildCount());

        final Datatype datatype = fileFormat.createDatatype(Datatype.CLASS_INTEGER, 4, Datatype.ORDER_NONE,
                Datatype.SIGN_2);
        final long[] dims = new long[] {1};
        final long[] maxdims = new long[] {1};

        // if the slice_thickness variable is not set to zero, add an acquisition node
        if (image.getFileInfo()[0].getSliceThickness() != 0f) {
            final H5ScalarDS acquisitionObject = (H5ScalarDS) fileFormat.createScalarDS("acquisition", infoGroup,
                    datatype, dims, maxdims, null, 0, null);
            final double slice_thickness = image.getFileInfo()[0].getSliceThickness();
            final Datatype dType = fileFormat.createDatatype(Datatype.CLASS_FLOAT, 8, Datatype.NATIVE,
                    Datatype.SIGN_NONE);
            final Attribute sliceThicknessAttr = new Attribute("slice_thickness", dType, dims);
            sliceThicknessAttr.setValue(new double[] {slice_thickness});
            acquisitionObject.writeMetadata(sliceThicknessAttr);
        }

        final String modality = FileMinc.getMincModality(image.getFileInfo()[0].getModality());
        if (modality != null) {
            final H5ScalarDS studyObject = (H5ScalarDS) fileFormat.createScalarDS("study", infoGroup, datatype, dims,
                    maxdims, null, 0, null);
            final Datatype dType = fileFormat.createDatatype(Datatype.CLASS_STRING, modality.length() + 1,
                    Datatype.NATIVE, -1);
            final Attribute modalityAttr = new Attribute("modality", dType, dims);
            modalityAttr.setValue(new String[] {modality});
            studyObject.writeMetadata(modalityAttr);
        }

        // build the dicom nodes
        final Hashtable<FileDicomKey, FileDicomTag> dTable = fileInfo.getDicomTable();
        final Enumeration<FileDicomKey> e = dTable.keys();

        final Hashtable<String, H5ScalarDS> groupTable = new Hashtable<String, H5ScalarDS>();

        // create nodes for each unique group
        while (e.hasMoreElements()) {
            final FileDicomKey tagKey = e.nextElement();
            final FileDicomTag dicomTag = dTable.get(tagKey);
            if (groupTable.get(tagKey.getGroup()) == null) {
                final H5ScalarDS dicomGroupObject = (H5ScalarDS) fileFormat.createScalarDS(
                        FileMincHDF.DICOM_GROUP_PREFIX + tagKey.getGroup(), infoGroup, datatype, dims, maxdims, null,
                        0, null);
                groupTable.put(tagKey.getGroup(), dicomGroupObject);
            }

            final H5ScalarDS dGroupObj = groupTable.get(tagKey.getGroup());
            // add the attribute to the dicom group

            final String[] elementStr = new String[] {dicomTag.getValue(true).toString()};
            final Datatype dType = fileFormat.createDatatype(Datatype.CLASS_STRING, elementStr[0].length() + 1,
                    Datatype.NATIVE, -1);
            final long[] attrDims = {1};
            final Attribute elementAttr = new Attribute(FileMincHDF.DICOM_ELEMENT_PREFIX + tagKey.getElement(), dType,
                    attrDims);
            elementAttr.setValue(elementStr);
            dGroupObj.writeMetadata(elementAttr);
        }

        return;
    }

    /**
     * Builds the image node (done for all filetypes when writing to MINC2.0 HDF5
     * 
     * @param image the image node
     * @param format the HDF5 FileFormat
     * @param mincNode the minc root node
     * @param model the tree model linked to the root node
     * @throws Exception
     */
    private void buildImageNode(final ModelImage image, final FileWriteOptions options, final FileFormat format,
            final HDFNode mincNode, final DefaultTreeModel model) throws Exception {
        // create the image node (root of image)
        HDFNode imageNode = null;
        final Group imageGroup = format.createGroup("image", (Group) mincNode.getUserObject());

        imageNode = new HDFNode(imageGroup);
        model.insertNodeInto(imageNode, mincNode, 1);

        // create the image number node (only 1, named "0")
        final Group imageNumGroup = format.createGroup("0", (Group) imageNode.getUserObject());
        final HDFNode imageNumNode = new HDFNode(imageNumGroup);
        model.insertNodeInto(imageNumNode, imageNode, imageNode.getChildCount());

        // create the actual image data node (leaf)

        // determine the class type
        final int mDataType = image.getType();
        int tclass = 0;
        int tsign = 0;
        int tsize = 0;
        switch (mDataType) {
            case ModelStorageBase.UBYTE:
                tclass = Datatype.CLASS_INTEGER;
                tsize = 1;
                tsign = Datatype.SIGN_NONE;
                break;
            case ModelStorageBase.BYTE:
                tclass = Datatype.CLASS_INTEGER;
                tsize = 1;
                tsign = Datatype.SIGN_2;
                break;
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
            case ModelStorageBase.FLOAT:
                tclass = Datatype.CLASS_FLOAT;
                tsize = 4;
                tsign = Datatype.SIGN_2;
                break;
            case ModelStorageBase.DOUBLE:
                tclass = Datatype.CLASS_FLOAT;
                tsize = 8;
                tsign = Datatype.SIGN_2;
                break;
            default:
                System.err.println("First switch(mDataType) not valid yet mDataType = " + mDataType);
                Preferences.debug("First switch(mDataType) not valid mDataType = " + mDataType + "\n");
                return;
        }

        int torder = 0;
        if (image.getFileInfo()[0].getEndianess()) {
            torder = Datatype.ORDER_BE;
        } else {
            torder = Datatype.ORDER_LE;
        }

        final Datatype datatype = fileFormat.createDatatype(tclass, tsize, torder, tsign);

        long[] dims = new long[image.getExtents().length];
        long[] maxdims = new long[dims.length];
        if (dims.length == 2) {
            dims[0] = image.getExtents()[0];
            dims[1] = image.getExtents()[1];
            maxdims[0] = dims[0];
            maxdims[1] = dims[1];
        } else {
            // order is z, y, x
            dims[0] = image.getExtents()[2];
            dims[1] = image.getExtents()[1];
            dims[2] = image.getExtents()[0];

            maxdims[0] = dims[0];
            maxdims[1] = dims[1];
            maxdims[2] = dims[2];
        }

        // create the Dataset (H5ScalarDS) that will hold the image data
        final H5ScalarDS imageObj = (H5ScalarDS) fileFormat.createScalarDS("image", imageNumGroup, datatype, dims,
                maxdims, null, 0, null);
        final DefaultMutableTreeNode imageDataNode = new DefaultMutableTreeNode(imageObj);
        model.insertNodeInto(imageDataNode, imageNumNode, imageNumNode.getChildCount());

        imageObj.init();

        // import the data
        final long[] start = imageObj.getStartDims(); // the starting dims

        final int sliceSize = image.getSliceSize();
        int numImages = 1;
        if (image.getExtents().length > 2) {
            numImages = image.getExtents()[2];
        }

        Object dataImportObj = null;
        switch (mDataType) {
            case ModelStorageBase.UBYTE:
            case ModelStorageBase.BYTE:
                dataImportObj = new byte[sliceSize];
                break;
            case ModelStorageBase.USHORT:
            case ModelStorageBase.SHORT:
                dataImportObj = new short[sliceSize];
                break;
            case ModelStorageBase.UINTEGER:
            case ModelStorageBase.INTEGER:
                dataImportObj = new int[sliceSize];
                break;
            case ModelStorageBase.FLOAT:
                dataImportObj = new float[sliceSize];
                break;
            case ModelStorageBase.DOUBLE:
                dataImportObj = new double[sliceSize];
                break;
            default:
                System.err.println("Second switch(mDataType) not valid yet mDataType = " + mDataType);
                Preferences.debug("Second switch(mDataType) not valid mDataType = " + mDataType + "\n");
                return;
        }

        for (int j = 0; j < numImages; j++) {
            start[0] = j;
            // System.err.println(imageData.getStartDims()[0]);
            // data = imageData.read();
            switch (mDataType) {
                case ModelStorageBase.UBYTE:
                case ModelStorageBase.BYTE:
                    image.exportData(j * sliceSize, sliceSize, (byte[]) dataImportObj);
                    break;
                case ModelStorageBase.USHORT:
                case ModelStorageBase.SHORT:
                    image.exportData(j * sliceSize, sliceSize, (short[]) dataImportObj);
                    break;
                case ModelStorageBase.UINTEGER:
                case ModelStorageBase.INTEGER:
                    image.exportData(j * sliceSize, sliceSize, (int[]) dataImportObj);
                    break;
                case ModelStorageBase.FLOAT:
                    image.exportData(j * sliceSize, sliceSize, (float[]) dataImportObj);
                    break;
                case ModelStorageBase.DOUBLE:
                    image.exportData(j * sliceSize, sliceSize, (double[]) dataImportObj);
                    break;
                default:
                    System.err.println("Third switch(mDataType) not valid yet mDataType = " + mDataType);
                    Preferences.debug("j = " + j + "\n");
                    Preferences.debug("Third switch(mDataType) not valid mDataType = " + mDataType + "\n");
                    return;
            }
            imageObj.write(dataImportObj);
            fireProgressStateChanged(Math.round(5 + ((float) j / numImages) * 95));
        }

        // dimorder based on orientation
        // orientation of the image.
        String dimOrder = new String();
        switch (options.getAxisOrientation()[2]) {

            case FileInfoBase.ORI_L2R_TYPE:
            case FileInfoBase.ORI_R2L_TYPE:
                dimOrder += "xspace";
                break;

            case FileInfoBase.ORI_A2P_TYPE:
            case FileInfoBase.ORI_P2A_TYPE:
                dimOrder += "yspace";
                break;

            case FileInfoBase.ORI_I2S_TYPE:
            case FileInfoBase.ORI_S2I_TYPE:
            default:
                dimOrder += "zspace";
        }

        switch (options.getAxisOrientation()[1]) {

            case FileInfoBase.ORI_L2R_TYPE:
            case FileInfoBase.ORI_R2L_TYPE:
                dimOrder += ",xspace";
                break;

            case FileInfoBase.ORI_A2P_TYPE:
            case FileInfoBase.ORI_P2A_TYPE:
                dimOrder += ",yspace";
                break;

            case FileInfoBase.ORI_I2S_TYPE:
            case FileInfoBase.ORI_S2I_TYPE:
                dimOrder += ",zspace";
                break;

            default:
                dimOrder += ",yspace";
        }

        switch (options.getAxisOrientation()[0]) {

            case FileInfoBase.ORI_L2R_TYPE:
            case FileInfoBase.ORI_R2L_TYPE:
                dimOrder += ",xspace";
                break;

            case FileInfoBase.ORI_A2P_TYPE:
            case FileInfoBase.ORI_P2A_TYPE:
                dimOrder += ",yspace";
                break;

            case FileInfoBase.ORI_I2S_TYPE:
            case FileInfoBase.ORI_S2I_TYPE:
                dimOrder += ",zspace";
                break;

            default:
                dimOrder += ",xspace";
        }

        // write dimorder Attribute to the node
        final String[] dimOrderString = new String[] {dimOrder};
        Datatype dType = fileFormat.createDatatype(Datatype.CLASS_STRING, dimOrderString[0].length() + 1, -1, -1);
        final long[] attrDims = {1};
        Attribute attr = new Attribute("dimorder", dType, attrDims);
        attr.setValue(dimOrderString);
        imageObj.writeMetadata(attr);

        // write the varid attribute
        final String[] varIDString = new String[] {"MINC standard variable"};
        dType = fileFormat.createDatatype(Datatype.CLASS_STRING, varIDString[0].length() + 1, Datatype.NATIVE, -1);
        attrDims[0] = 1;
        final Attribute varIDAttr = new Attribute("varid", dType, attrDims);
        varIDAttr.setValue(varIDString);
        imageObj.writeMetadata(varIDAttr);

        // write the vartype attribute
        final String[] varTypeString = new String[] {"group________"};
        dType = fileFormat.createDatatype(Datatype.CLASS_STRING, varIDString[0].length() + 1, Datatype.NATIVE, -1);
        attrDims[0] = 1;
        attr = new Attribute("vartype", dType, attrDims);
        attr.setValue(varTypeString);
        imageObj.writeMetadata(attr);

        // write the version attribute
        final String[] versionString = new String[] {"MINC Version    1.0"};
        dType = fileFormat.createDatatype(Datatype.CLASS_STRING, versionString[0].length() + 1, Datatype.NATIVE, -1);
        attrDims[0] = 1;
        final Attribute versionAttr = new Attribute("version", dType, attrDims);
        versionAttr.setValue(versionString);
        imageObj.writeMetadata(versionAttr);

        // write complete Attribute to the node
        final String[] completeString = new String[] {"true_"};
        dType = fileFormat.createDatatype(Datatype.CLASS_STRING, completeString[0].length() + 1, -1, -1);
        attrDims[0] = 1;
        attr = new Attribute("complete", dType, attrDims);
        attr.setValue(completeString);
        imageObj.writeMetadata(attr);

        // write valid_range Attribute to the node
        dType = fileFormat.createDatatype(Datatype.CLASS_FLOAT, 8, Datatype.NATIVE, Datatype.SIGN_2);
        attrDims[0] = 2;
        attr = new Attribute("valid_range", dType, attrDims);
        if (image.getFileInfo()[0] instanceof FileInfoMincHDF) {
            attr.setValue( ((FileInfoMincHDF) image.getFileInfo()[0]).getValidRange());
        } else {
            attr.setValue(new double[] {image.getMin(), image.getMax()});
            // not supported yet
        }
        imageObj.writeMetadata(attr);

        // create the image-max and image-min arrays (always 64bit float?)
        tsize = 8;
        final Datatype imageRangeDatatype = fileFormat.createDatatype(Datatype.CLASS_FLOAT, tsize, -1, Datatype.SIGN_2);
        dims = new long[1];
        maxdims = new long[1];
        dims[0] = 1;
        if (image.getExtents().length > 2) {
            dims[0] = image.getExtents()[2];
        }
        maxdims[0] = dims[0];

        final H5ScalarDS imageMaxObj = (H5ScalarDS) fileFormat.createScalarDS("image-max", imageNumGroup,
                imageRangeDatatype, dims, maxdims, null, 0, null);
        final DefaultMutableTreeNode imageMaxNode = new DefaultMutableTreeNode(imageMaxObj);
        model.insertNodeInto(imageMaxNode, imageNumNode, imageNumNode.getChildCount());

        final H5ScalarDS imageMinObj = (H5ScalarDS) fileFormat.createScalarDS("image-min", imageNumGroup,
                imageRangeDatatype, dims, maxdims, null, 0, null);
        final DefaultMutableTreeNode imageMinNode = new DefaultMutableTreeNode(imageMinObj);
        model.insertNodeInto(imageMinNode, imageNumNode, imageNumNode.getChildCount());

        final double[] imageMax = new double[(int) dims[0]];
        final double[] imageMin = new double[(int) dims[0]];

        double rescaleIntercept = 0;
        double rescaleSlope = 0;

        final double[] imageSliceBuffer = new double[image.getSliceSize()];

        for (int i = 0; i < dims[0]; i++) {
            image.exportData(i * sliceSize, sliceSize, imageSliceBuffer);
            rescaleSlope = image.getFileInfo()[i].getRescaleSlope();
            rescaleIntercept = image.getFileInfo()[i].getRescaleIntercept();

            imageMax[i] = imageSliceBuffer[0];
            imageMin[i] = imageSliceBuffer[0];
            for (final double element : imageSliceBuffer) {
                if (element > imageMax[i]) {
                    imageMax[i] = element;
                } else if (element < imageMin[i]) {
                    imageMin[i] = element;
                }
            }
            imageMax[i] = (imageMax[i] * rescaleSlope) + rescaleIntercept;
            imageMin[i] = (imageMin[i] * rescaleSlope) + rescaleIntercept;

        }
        // init and then read in the two arrays
        imageMaxObj.init();
        imageMinObj.init();

        imageMaxObj.write(imageMax);
        imageMinObj.write(imageMin);

        // write the dimorder attribute to the image-max and image-min nodes
        final String[] rangeDimString = new String[] {"zspace"};
        dType = fileFormat.createDatatype(Datatype.CLASS_STRING, rangeDimString[0].length() + 1, Datatype.NATIVE, -1);
        attrDims[0] = 1;
        attr = new Attribute("dimorder", dType, attrDims);
        attr.setValue(rangeDimString);

        final String[] varTypeMMString = new String[] {"var_attribute"};
        dType = fileFormat.createDatatype(Datatype.CLASS_STRING, varTypeMMString[0].length() + 1, Datatype.NATIVE, -1);
        attrDims[0] = 1;
        final Attribute varTypeAttr = new Attribute("vartype", dType, attrDims);
        varTypeAttr.setValue(varTypeMMString);

        imageMaxObj.writeMetadata(attr);
        imageMaxObj.writeMetadata(varIDAttr);
        imageMaxObj.writeMetadata(varTypeAttr);
        imageMaxObj.writeMetadata(versionAttr);

        imageMinObj.writeMetadata(attr);
        imageMinObj.writeMetadata(varIDAttr);
        imageMinObj.writeMetadata(varTypeAttr);
        imageMinObj.writeMetadata(versionAttr);
    }

    /**
     * Writes a MINC format type image. Calls the appropriate header method.
     * 
     * @param image Image model where the data is stored.
     * @param options Information about how to write this file.
     * 
     * @exception IOException if there is an error writing the file
     * 
     * @see FileInfoMinc
     * @see FileMinc
     */
    public void writeImage(final ModelImage image, final FileWriteOptions options) throws Exception {

        Preferences.debug("Entering writeImage\n");
        final FileInfoBase fileInfo = image.getFileInfo()[0];
        final String fullPath = fileDir + File.separator + fileName;

        // create the h5File and open it
        final H5File h5File = new H5File(fullPath, FileFormat.WRITE);

        final FileFormat format = h5File.createFile(fullPath, FileFormat.FILE_CREATE_DELETE);
        h5File.open();

        // create the fileRoot (H5File)
        final DefaultMutableTreeNode fileRoot = (DefaultMutableTreeNode) h5File.getRootNode();
        final DefaultTreeModel model = new DefaultTreeModel(fileRoot);

        // create the minc 2.0 group
        final Group mincGroup = format.createGroup("minc-2.0", (Group) fileRoot.getUserObject());

        final HDFNode mincNode = new HDFNode(mincGroup);
        model.insertNodeInto(mincNode, fileRoot, fileRoot.getChildCount());

        // if image was MINC_HDF, the first slice's fileinfo contains the dimension and info nodes
        if (fileInfo instanceof FileInfoMincHDF) {
            final DefaultMutableTreeNode dimNode = ((FileInfoMincHDF) fileInfo).getDimensionNode();
            format.copy((HObject) dimNode.getUserObject(), mincGroup, "dimensions");

            model.insertNodeInto(dimNode, mincNode, mincNode.getChildCount());

            final DefaultMutableTreeNode infoNode = ((FileInfoMincHDF) fileInfo).getInfoNode();
            format.copy((HObject) infoNode.getUserObject(), mincGroup, "info");
            model.insertNodeInto(infoNode, mincNode, mincNode.getChildCount());

        } else {
            // image was not MINC_HDF to begin with, so must create the dimension and info nodes

            buildDimensionNode(image, options, format, mincNode, model);

            buildInfoNode(image, format, mincNode, model);
        }

        Preferences.debug("Entering buildImageNode\n");
        buildImageNode(image, options, format, mincNode, model);

        h5File.close();
    }

    public class HDFNode extends DefaultMutableTreeNode {
        public static final long serialVersionUID = HObject.serialVersionUID;

        public HDFNode(final Group group) {
            super(group);
        }

        public boolean isLeaf() {
            return false;
        }
    }
}
