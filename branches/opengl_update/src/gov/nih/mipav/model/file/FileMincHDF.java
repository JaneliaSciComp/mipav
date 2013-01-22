package gov.nih.mipav.model.file;


import gov.nih.mipav.model.file.FileDicomTagInfo.NumType;
import gov.nih.mipav.model.file.FileDicomTagInfo.StringType;
import gov.nih.mipav.model.file.FileDicomTagInfo.VR;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;
import java.util.*;

import javax.swing.tree.*;

import ncsa.hdf.object.*;
import ncsa.hdf.object.h5.*;


/**
 * HDF5 based reader/writer for MINC 2.0
 * 
 * 
 * Currently, for the image max and image min nodes: in the case of 3d images in which 1 image min and 1 image max is to
 * be written out per slice, the dimorder is hardcoded to zspace. Sometimes minc2 3D images come in with just 1 image
 * min and 1 image max per volume...in that case, we write out the same way and do not write out a dimorder
 * 
 * ToDo: We need to make this type of functionality work for 4D images. Currently the code writes out 1 image min nd 1
 * image max per slice and hardcode the dimorder to time,zspace. We need to write out the same way we read in....yet to
 * do.
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

    public static final String LEAF_T_SPACE = "time";

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
    private final String fileDir;

    /** The name of the minc file to be read in or written out. */
    private final String fileName;

    /** The ordering of the dimensions. Should be found in the image node attributes. */
    private String[] dimOrder;

    /** will always use the HDF5 fileformat */
    private final FileFormat fileFormat = FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5);

    // private FileFormat fileFormat;

    /** the file to be read/written */
    private H5File h5File;

    private final FileInfoMincHDF fileInfo;

    private int[] dimReorderIndexes = null;

    private boolean is4D;

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
     * parses the dim order
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
        } else if (imageNode.getUserObject() == null) {
            System.out.println("imageNode.getUserObject() == null");
        }
        imageMetaData = ((HObject) imageNode.getUserObject()).getMetadata();

        final Iterator<Attribute> it = imageMetaData.iterator();
        while (it.hasNext()) {
            final Attribute imageAttr = it.next();
            if (imageAttr.getName().equals(FileMincHDF.ATTR_IMAGE_DIM_ORDER)) {
                final String dimOrderString = ((String[]) imageAttr.getValue())[0];
                if (dimOrderString.startsWith("zspace")) {
                    fileInfo.setImageOrientation(FileInfoBase.AXIAL);
                } else if (dimOrderString.startsWith("xspace")) {
                    fileInfo.setImageOrientation(FileInfoBase.SAGITTAL);
                } else if (dimOrderString.startsWith("yspace")) {
                    fileInfo.setImageOrientation(FileInfoBase.CORONAL);
                } else if (dimOrderString.startsWith("time")) {
                    is4D = true;
                    final String dimOrderString_sub = dimOrderString.substring(dimOrderString.indexOf(",") + 1);
                    if (dimOrderString_sub.startsWith("zspace")) {
                        fileInfo.setImageOrientation(FileInfoBase.AXIAL);
                    } else if (dimOrderString_sub.startsWith("xspace")) {
                        fileInfo.setImageOrientation(FileInfoBase.SAGITTAL);
                    } else if (dimOrderString_sub.startsWith("yspace")) {
                        fileInfo.setImageOrientation(FileInfoBase.CORONAL);
                    }
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
        // initialize the dirCosines to be identity in case it is not specified in the header
        for (int i = 0; i < totalDims; i++) {
            dirCosines[i][i] = 1;
        }
        dimStringsReordered = new String[totalDims];
        dimStrings = new String[totalDims];

        dimReorderIndexes = new int[dimOrder.length];

        if (is4D) {
            for (int i = 0; i < dimOrder.length; i++) {
                if (dimOrder[i].equals(FileMincHDF.LEAF_X_SPACE)) {
                    dimReorderIndexes[i] = 2;

                } else if (dimOrder[i].equals(FileMincHDF.LEAF_Y_SPACE)) {
                    dimReorderIndexes[i] = 1;

                } else if (dimOrder[i].equals(FileMincHDF.LEAF_Z_SPACE)) {
                    dimReorderIndexes[i] = 0;
                } else if (dimOrder[i].equals(FileMincHDF.LEAF_T_SPACE)) {
                    dimReorderIndexes[i] = 3;

                }
            }
        } else {
            for (int i = 0; i < dimOrder.length; i++) {
                if (dimOrder[i].equals(FileMincHDF.LEAF_X_SPACE)) {
                    dimReorderIndexes[i] = 2;

                } else if (dimOrder[i].equals(FileMincHDF.LEAF_Y_SPACE)) {
                    dimReorderIndexes[i] = 1;

                } else if (dimOrder[i].equals(FileMincHDF.LEAF_Z_SPACE)) {
                    dimReorderIndexes[i] = 0;

                }
            }
        }

        // following is needed to determine the order of the axes
        // this is needed when setting up the axes orientations in mipav
        // which uses the dimString and the step
        for (int i = 0; i < dimensionNode.getChildCount(); i++) {
            currentDimNode = (DefaultMutableTreeNode) dimensionNode.getChildAt(i);

            final int order = getMipavOrderFromDimNode(currentDimNode.toString());

            dimStringsReordered[order] = currentDimNode.toString();
            dimStrings[dimReorderIndexes[i]] = currentDimNode.toString();
            final List<Attribute> metaData = ((HObject) currentDimNode.getUserObject()).getMetadata();
            final Iterator<Attribute> it = metaData.iterator();
            while (it.hasNext()) {
                final Attribute attr = it.next();
                final String attrName = attr.getName();

                if (attrName.equals(FileMincHDF.ATTR_DIM_UNITS)) {
                    units[dimReorderIndexes[i]] = (getUnitFromString(((String[]) attr.getValue())[0])).getLegacyNum();
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
                            final String keyStr = (dicomGroup + "," + dicomElement).toUpperCase();

                            final long dataLen = attr.getDataDims()[0];

                            final Datatype dataType = attr.getType();

                            String value;
                            if (dataType.getDatatypeClass() == Datatype.CLASS_CHAR) {
                                final byte[] bytes = (byte[]) attr.getValue();

                                final FileDicomKey key = new FileDicomKey(keyStr);
                                final VR dicomTypeStr = DicomDictionary.getType(key);
                                if (dicomTypeStr == null) {
                                    // TODO: better way to handle private tags?
                                    /*
                                     * if (dataType.isUnsigned()) { value = "" + (bytes[0] & 0xFF); } else { value = "" +
                                     * bytes[0]; }
                                     * 
                                     * for (int j = 1; j < dataLen; j++) { if (dataType.isUnsigned()) { value += ", " +
                                     * (bytes[j] & 0xFF); } else { value += ", " + bytes[j]; } }
                                     */
                                    // TODO: skipping all private tags, for now
                                    System.err.println("Skipping private tag: " + keyStr);
                                    continue;
                                } else if (dicomTypeStr.getType().equals(NumType.LONG)) {
                                    final int vm = DicomDictionary.getVM(key);
                                    value = "" + FileBase.bytesToInt(false, 0, bytes);
                                    for (int j = 1; j < vm; j++) {
                                        value += ", " + FileBase.bytesToInt(false, j * 4, bytes);
                                    }
                                } else if (dicomTypeStr.getType().equals(NumType.SHORT)) {
                                    final int vm = DicomDictionary.getVM(key);
                                    value = "" + FileBase.bytesToShort(false, 0, bytes);
                                    for (int j = 1; j < vm; j++) {
                                        value += ", " + FileBase.bytesToShort(false, j * 2, bytes);
                                    }
                                } else if (dicomTypeStr.getType().equals(NumType.DOUBLE)) {
                                    final int vm = DicomDictionary.getVM(key);
                                    value = "" + FileBase.bytesToDouble(false, 0, bytes);
                                    for (int j = 1; j < vm; j++) {
                                        value += ", " + FileBase.bytesToDouble(false, j * 8, bytes);
                                    }
                                } else if (dicomTypeStr.getType().equals(NumType.FLOAT)) {
                                    final int vm = DicomDictionary.getVM(key);
                                    value = "" + FileBase.bytesToFloat(false, 0, bytes);
                                    for (int j = 1; j < vm; j++) {
                                        value += ", " + FileBase.bytesToFloat(false, j * 4, bytes);
                                    }
                                } else if (dicomTypeStr.getType() instanceof StringType) {
                                    value = new String(bytes);
                                } else if (dicomTypeStr.equals(VR.SQ)) {
                                    // TODO: convert bytes to sequence... for now, we remove them until we handle them
                                    System.err.println("Skipped DICOM sequence: " + keyStr);
                                    continue;
                                } else {
                                    // TODO: TYPE_UNKNOWN
                                    System.err.println("Unknown type for tag: " + keyStr);
                                    continue;
                                }
                            } else if (dataType.getDatatypeClass() == Datatype.CLASS_STRING) {
                                final String[] strArray = (String[]) attr.getValue();
                                value = strArray[0];

                                for (int j = 1; j < dataLen; j++) {
                                    value += ", " + strArray;
                                }
                            } else {
                                System.err.println("Unsupported minc2 attribute datatype for tag: " + keyStr);
                                continue;
                            }

                            fileInfo.getDicomTable().put(keyStr, value);
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
        int numVols = 1;
        int sliceSize;
        // some minc2 images will only have 1 image min and 1 image max....in this case the minMaxDimOrder is not
        // set..so it is false
        // TODO: make this work for 4D images
        boolean hasImageMinMaxDimOrder = false;
        fileInfo.setImageNode(childNode);

        for (int i = 0; i < childNode.getChildCount(); i++) {
            currentNode = (DefaultMutableTreeNode) childNode.getChildAt(i);
            if (currentNode.getUserObject().toString().equals(FileMincHDF.LEAF_IMAGE)) {
                // parse the image data here

                // determine the Datatype class (int)
                final int dataType = ((H5ScalarDS) currentNode.getUserObject()).getDatatype().getDatatypeClass();
                // final int sign = ((H5ScalarDS) currentNode.getUserObject()).getDatatype().getDatatypeSign();
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
                        } else if (dimOrder.startsWith("time")) {
                            final String dimOrderString_sub = dimOrder.substring(dimOrder.indexOf(",") + 1);
                            if (dimOrderString_sub.startsWith("zspace")) {
                                fileInfo.setImageOrientation(FileInfoBase.AXIAL);
                            } else if (dimOrderString_sub.startsWith("xspace")) {
                                fileInfo.setImageOrientation(FileInfoBase.SAGITTAL);
                            } else if (dimOrderString_sub.startsWith("yspace")) {
                                fileInfo.setImageOrientation(FileInfoBase.CORONAL);
                            }
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

                final boolean is3D = (imageData.getRank() == 3) && ! ((ScalarDS) imageData).isTrueColor();

                image = new ModelImage(fileInfo.getDataType(), fileInfo.getExtents(), fileName);

                final long[] dims = imageData.getDims();
                int[] selectedIndex = imageData.getSelectedIndex();

                // set of the size of the slice of data we want to read in each time
                final long[] selected = imageData.getSelectedDims();

                if (is3D) {
                    numImages = fileInfo.getExtents()[2];

                    for (int di = 0; di < dims.length; di++) {
                        final int mipavOrder = getMipavOrderFromDimNode(dimStrings[di]);
                        selectedIndex[mipavOrder] = di;
                        selected[mipavOrder] = dims[mipavOrder];
                    }

                    // set the out-of-plane selection size to 1
                    selected[selectedIndex[2]] = 1;
                } else if (is4D) {
                    numVols = fileInfo.getExtents()[3];
                    numImages = fileInfo.getExtents()[2] * fileInfo.getExtents()[3];

                    for (int di = 0; di < dims.length; di++) {
                        final int mipavOrder = getMipavOrderFromDimNode(dimStrings[di]);
                        try {
                            selectedIndex[mipavOrder] = di;
                        }
                        catch(ArrayIndexOutOfBoundsException e) {
                            int len = selectedIndex.length;
                            int itemp[] = new int[len];
                            for (int j = 0; j < len; j++) {
                                itemp[j] = selectedIndex[j];
                            }
                            selectedIndex = new int[len+1];
                            selectedIndex[mipavOrder] = di;
                        }
                        selected[mipavOrder] = dims[mipavOrder];

                        selected[selectedIndex[3]] = 1;
                    }
                }

                Object data = null;

                int numParts;
                if (is4D) {
                    numParts = numVols;
                    sliceSize = fileInfo.getExtents()[0] * fileInfo.getExtents()[1];
                }
                else {
                    numParts = numImages;
                    sliceSize = imageData.getWidth() * imageData.getHeight();
                }
                
                // start dims array seems important for 3D images, but not 4D. 3D reading requires incrementing it
                long[] start = imageData.getStartDims(); // the starting dims - should be 0,0,0,... to start

                for (int j = 0; j < numParts; j++) {
                    if (is4D) {
                        data = imageData.read();

                        if (fileInfo.getDataType() == ModelStorageBase.SHORT
                                || fileInfo.getDataType() == ModelStorageBase.USHORT) {
                            image.importData((fileInfo.getExtents()[2] * sliceSize *j), (short[])data, false);
                        } else if (fileInfo.getDataType() == ModelStorageBase.INTEGER
                                || fileInfo.getDataType() == ModelStorageBase.UINTEGER) {
                            image.importData((fileInfo.getExtents()[2] * sliceSize *j), (int[])data, false);
                        } else if (fileInfo.getDataType() == ModelStorageBase.BYTE
                                || fileInfo.getDataType() == ModelStorageBase.UBYTE) {
                            image.importData((fileInfo.getExtents()[2] * sliceSize *j), (byte[])data, false);
                        } else if (fileInfo.getDataType() == ModelStorageBase.FLOAT) {
                            image.importData((fileInfo.getExtents()[2] * sliceSize *j), (float[])data, false);
                        } else if (fileInfo.getDataType() == ModelStorageBase.DOUBLE) {
                            image.importData((fileInfo.getExtents()[2] * sliceSize *j), (double[])data, false);
                        }
                    } else {
                        start[0] = j;
                        data = imageData.read();
                        if (fileInfo.getDataType() == ModelStorageBase.SHORT
                                || fileInfo.getDataType() == ModelStorageBase.USHORT) {
                            image.importData(j * sliceSize, (short[]) data, false);
                        } else if (fileInfo.getDataType() == ModelStorageBase.INTEGER
                                || fileInfo.getDataType() == ModelStorageBase.UINTEGER) {
                            image.importData(j * sliceSize, (int[]) data, false);
                        } else if (fileInfo.getDataType() == ModelStorageBase.BYTE
                                || fileInfo.getDataType() == ModelStorageBase.UBYTE) {
                            image.importData(j * sliceSize, (byte[]) data, false);
                        } else if (fileInfo.getDataType() == ModelStorageBase.FLOAT) {
                            image.importData(j * sliceSize, (float[]) data, false);
                        } else if (fileInfo.getDataType() == ModelStorageBase.DOUBLE) {
                            image.importData(j * sliceSize, (double[]) data, false);
                        }
                    }

                    fireProgressStateChanged(Math.round(5 + ((float) j / numParts) * 95));
                }
            }

            else if (currentNode.getUserObject().toString().equals(FileMincHDF.LEAF_IMAGE_MAX)) {
                final H5ScalarDS data = (H5ScalarDS) currentNode.getUserObject();

                final List<Attribute> imageMetaData = ((HObject) currentNode.getUserObject()).getMetadata();
                final Iterator<Attribute> it = imageMetaData.iterator();
                while (it.hasNext()) {
                    final Attribute imageAttr = it.next();
                    final String attrName = imageAttr.getName();
                    if (attrName.equals(FileMincHDF.ATTR_IMAGE_DIM_ORDER)) {
                        hasImageMinMaxDimOrder = true;

                    }
                }
                data.init();
                imageMax = (double[]) data.getData();
            } else if (currentNode.getUserObject().toString().equals(FileMincHDF.LEAF_IMAGE_MIN)) {
                final H5ScalarDS data = (H5ScalarDS) currentNode.getUserObject();
                final List<Attribute> imageMetaData = ((HObject) currentNode.getUserObject()).getMetadata();
                final Iterator<Attribute> it = imageMetaData.iterator();
                while (it.hasNext()) {
                    final Attribute imageAttr = it.next();
                    final String attrName = imageAttr.getName();
                    if (attrName.equals(FileMincHDF.ATTR_IMAGE_DIM_ORDER)) {
                        hasImageMinMaxDimOrder = true;

                    }
                }
                data.init();
                imageMin = (double[]) data.getData();
            }

        }

        // create the array of fileinfos for the model image
        final FileInfoMincHDF[] fileInfos = new FileInfoMincHDF[numImages];
        fileInfos[0] = fileInfo;
        fileInfos[0].setHasImageMinMaxDimOrder(hasImageMinMaxDimOrder);

        double[] rescaleSlope = null;
        double[] rescaleIntercept = null;
        // if imageMax and imageMin were present, do the rescaleintercept here
        if (imageMax != null && imageMin != null && fileInfo.getValidRange() != null) {

            rescaleSlope = new double[numImages];
            rescaleIntercept = new double[numImages];
            FileInfoMincHDF.calculateRescaleIntercept(rescaleIntercept, rescaleSlope, imageMax, imageMin, fileInfo
                    .getValidRange(), hasImageMinMaxDimOrder);
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
            if (is4D) {
                final double[] step2 = {step[0], step[1], step[2]};
                final double[][] dirCosines2 = new double[3][3];
                for (int m = 0; m < 3; m++) {
                    dirCosines2[m][0] = dirCosines[m][0];
                    dirCosines2[m][1] = dirCosines[m][1];
                    dirCosines2[m][2] = dirCosines[m][2];
                }
                final boolean[] isCentered2 = {isCentered[0], isCentered[1], isCentered[2]};
                //final double[] mincStartLoc2 = {mincStartLoc[0], mincStartLoc[1], mincStartLoc[2]};
                fileInfos[0].setStartLocations(fileInfos[0].getConvertStartLocationsToDICOM(step2, dirCosines2,
                        isCentered2, 0, mincStartLoc));

            } else {
                fileInfos[0].setStartLocations(fileInfos[0].getConvertStartLocationsToDICOM(step, dirCosines,
                        isCentered, 0, mincStartLoc));
            }
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
                if (is4D) {
                    final double[] step2 = {step[0], step[1], step[2]};
                    final double[][] dirCosines2 = new double[3][3];
                    for (int m = 0; m < 3; m++) {
                        dirCosines2[m][0] = dirCosines[m][0];
                        dirCosines2[m][1] = dirCosines[m][1];
                        dirCosines2[m][2] = dirCosines[m][2];
                    }
                    final boolean[] isCentered2 = {isCentered[0], isCentered[1], isCentered[2]};
                    //final double[] mincStartLoc2 = {mincStartLoc[0], mincStartLoc[1], mincStartLoc[2]};
                    fileInfos[i].setStartLocations(fileInfos[i].getConvertStartLocationsToDICOM(step2, dirCosines2,
                            isCentered2, i, mincStartLoc));

                } else {
                    fileInfos[i].setStartLocations(fileInfos[i].getConvertStartLocationsToDICOM(step, dirCosines,
                            isCentered, i, mincStartLoc));
                }
            }

        }

        // Rescale actual image data
        byte[] byteBuff = null;
        short[] shortBuff = null;
        int[] intBuff = null;
        float[] floatBuff = null;
        double[] doubleBuff = null;
        short shortVal;
        int intVal;
        float floatVal;
        byte byteVal;
        double doubleVal;
        double slope, intercept;
        slope = 1.0;
        intercept = 0.0;
        float f;
        if (fileInfo.getDataType() == ModelStorageBase.SHORT || fileInfo.getDataType() == ModelStorageBase.USHORT) {
            shortBuff = new short[image.getExtents()[0] * image.getExtents()[1]];
        } else if (fileInfo.getDataType() == ModelStorageBase.INTEGER
                || fileInfo.getDataType() == ModelStorageBase.UINTEGER) {
            intBuff = new int[image.getExtents()[0] * image.getExtents()[1]];
        } else if (fileInfo.getDataType() == ModelStorageBase.BYTE || fileInfo.getDataType() == ModelStorageBase.UBYTE) {
            byteBuff = new byte[image.getExtents()[0] * image.getExtents()[1]];
        } else if (fileInfo.getDataType() == ModelStorageBase.FLOAT) {
            floatBuff = new float[image.getExtents()[0] * image.getExtents()[1]];
        } else if (fileInfo.getDataType() == ModelStorageBase.DOUBLE) {
            doubleBuff = new double[image.getExtents()[0] * image.getExtents()[1]];
        }
        for (int j = 0; j < numImages; j++) {
            slope = fileInfos[j].getRescaleSlope();
            intercept = fileInfos[j].getRescaleIntercept();
            if (fileInfo.getDataType() == ModelStorageBase.SHORT || fileInfo.getDataType() == ModelStorageBase.USHORT) {
                image.exportSliceXY(j, shortBuff);
                for (int k = 0; k < shortBuff.length; k++) {
                    shortVal = shortBuff[k];
                    f = (float) ( (shortVal * slope) + intercept);
                    shortBuff[k] = (short) f;
                }
                image.importData(j * image.getExtents()[0] * image.getExtents()[1], shortBuff, false);
            } else if (fileInfo.getDataType() == ModelStorageBase.INTEGER
                    || fileInfo.getDataType() == ModelStorageBase.UINTEGER) {
                image.exportSliceXY(j, intBuff);
                for (int k = 0; k < intBuff.length; k++) {
                    intVal = intBuff[k];
                    f = (float) ( (intVal * slope) + intercept);
                    intBuff[k] = (int) f;
                }
                image.importData(j * image.getExtents()[0] * image.getExtents()[1], intBuff, false);
            } else if (fileInfo.getDataType() == ModelStorageBase.BYTE
                    || fileInfo.getDataType() == ModelStorageBase.UBYTE) {
                image.exportSliceXY(j, byteBuff);
                for (int k = 0; k < byteBuff.length; k++) {
                    byteVal = byteBuff[k];
                    f = (float) ( (byteVal * slope) + intercept);
                    byteBuff[k] = (byte) f;
                }
                image.importData(j * image.getExtents()[0] * image.getExtents()[1], byteBuff, false);
            } else if (fileInfo.getDataType() == ModelStorageBase.FLOAT) {
                image.exportSliceXY(j, floatBuff);
                for (int k = 0; k < floatBuff.length; k++) {
                    floatVal = floatBuff[k];
                    f = (float) ( (floatVal * slope) + intercept);
                    floatBuff[k] = f;
                }
                image.importData(j * image.getExtents()[0] * image.getExtents()[1], floatBuff, false);
            } else if (fileInfo.getDataType() == ModelStorageBase.DOUBLE) {
                image.exportSliceXY(j, doubleBuff);
                for (int k = 0; k < doubleBuff.length; k++) {
                    doubleVal = doubleBuff[k];
                    f = (float) ( (doubleVal * slope) + intercept);
                    doubleBuff[k] = f;
                }
                image.importData(j * image.getExtents()[0] * image.getExtents()[1], doubleBuff, false);
            }
        }

        image.setFileInfo(fileInfos);
        image.calcMinMax();

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
        try {
            h5File = (H5File) fileFormat.createInstance(fileDir + fileName, FileFormat.READ);
            // h5File = new H5File(fileDir + fileName, FileFormat.READ);
            @SuppressWarnings("unused")
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

        boolean is4D = false;
        if (image.is4DImage()) {
            is4D = true;
        }
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

        if ( (image.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL))
                || (image.getFileInfo()[0].getFileFormat() == FileUtility.DICOM)) {
            Preferences.debug("Copying direction cosines from SCANNER ANATOMICAL matrix\n", Preferences.DEBUG_FILEIO);
            scannerArray = new double[3][3];
            for (int i = 0; i < 3; i++) {
                for (int j = 0; j < 3; j++) {
                    scannerArray[i][j] = image.getMatrix().get(i, j);
                    Preferences.debug("scannerArray[" + i + "][" + j + "] = " + scannerArray[i][j] + " ",
                    		Preferences.DEBUG_FILEIO);
                }
                Preferences.debug("\n", Preferences.DEBUG_FILEIO);
            }
            // MINC is L->R and P->A while MIPAV is R->L and A->P, so multiply first 2 rows by -1
            for (int i = 0; i <= 1; i++) {
                for (int j = 0; j < 3; j++) {
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

        double xstart, ystart, zstart;
        double xstep, ystep, zstep;
        double tstart = 0;
        double tstep = 1;

        if (is4D) {
            tstart = options.getTStart();
            tstep = options.getTSpace();
        }

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
        } else {
            dirCosines = new double[] {1, 0, 0};
        }
        dirCosinesAttr.setValue(dirCosines);
        xSpaceObj.writeMetadata(dirCosinesAttr);

        // units
        unitsString = new String[] {Unit.getUnitFromLegacyNum(image.getFileInfo()[0].getUnitsOfMeasure(0)).getAbbrev()};
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
        } else {
            dirCosines = new double[] {0, 1, 0};
        }
        dType = fileFormat.createDatatype(Datatype.CLASS_FLOAT, 8, Datatype.NATIVE, Datatype.SIGN_NONE);
        attrDims[0] = 3;
        dirCosinesAttr = new Attribute("direction_cosines", dType, attrDims);
        dirCosinesAttr.setValue(dirCosines);
        ySpaceObj.writeMetadata(dirCosinesAttr);

        // units
        attrDims[0] = 1;

        unitsString = new String[] {Unit.getUnitFromLegacyNum(image.getFileInfo()[0].getUnitsOfMeasure(1)).getAbbrev()};
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
            final int endSlice = options.getEndSlice();
            final int beginSlice = options.getBeginSlice();

            // ZSPACE NODE
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
            } else {
                dirCosines = new double[] {0, 0, 1};
            }
            dType = fileFormat.createDatatype(Datatype.CLASS_FLOAT, 8, Datatype.NATIVE, Datatype.SIGN_NONE);
            attrDims[0] = 3;
            dirCosinesAttr = new Attribute("direction_cosines", dType, attrDims);
            dirCosinesAttr.setValue(dirCosines);
            zSpaceObj.writeMetadata(dirCosinesAttr);

            // units
            attrDims[0] = 1;
            unitsString = new String[] {Unit.getUnitFromLegacyNum(image.getFileInfo()[0].getUnitsOfMeasure(1))
                    .getAbbrev()};
            dType = fileFormat.createDatatype(Datatype.CLASS_STRING, alignmentString[0].length() + 1, -1, -1);
            unitsAttr = new Attribute("units", dType, attrDims);
            unitsAttr.setValue(unitsString);
            zSpaceObj.writeMetadata(unitsAttr);

            // spacetype
            zSpaceObj.writeMetadata(spaceTypeAttr);

            // length
            length = new int[] {(endSlice - beginSlice) + 1};
            lengthAttr.setValue(length);
            zSpaceObj.writeMetadata(lengthAttr);

            if (is4D) {
                final int endTime = options.getEndTime();
                final int beginTime = options.getBeginTime();

                // ZSPACE NODE
                final H5ScalarDS tSpaceObj = (H5ScalarDS) fileFormat.createScalarDS("time", dimGroup, datatype, dims,
                        maxdims, null, 0, null);
                final DefaultMutableTreeNode tSpaceNode = new DefaultMutableTreeNode(tSpaceObj);
                model.insertNodeInto(tSpaceNode, dimNode, dimNode.getChildCount());

                // varid
                zSpaceObj.writeMetadata(varIDAttr);

                // vartype
                zSpaceObj.writeMetadata(varTypeAttr);

                // version
                zSpaceObj.writeMetadata(versionAttr);

                // alignment
                zSpaceObj.writeMetadata(alignmentAttr);

                // step (resolution)
                dType = fileFormat.createDatatype(Datatype.CLASS_FLOAT, 8, Datatype.NATIVE, Datatype.SIGN_NONE);
                stepAttr = new Attribute("step", dType, attrDims);
                stepAttr.setValue(new double[] {tstep});
                tSpaceObj.writeMetadata(stepAttr);

                // start (origin)
                startAttr.setValue(new double[] {tstart});
                tSpaceObj.writeMetadata(startAttr);

                // units
                attrDims[0] = 1;
                unitsString = new String[] {Unit.getUnitFromLegacyNum(image.getFileInfo()[0].getUnitsOfMeasure(3))
                        .getAbbrev()};
                dType = fileFormat.createDatatype(Datatype.CLASS_STRING, alignmentString[0].length() + 1, -1, -1);
                unitsAttr = new Attribute("units", dType, attrDims);
                unitsAttr.setValue(unitsString);
                tSpaceObj.writeMetadata(unitsAttr);

                // spacetype
                tSpaceObj.writeMetadata(spaceTypeAttr);

                // length
                length = new int[] {(endTime - beginTime) + 1};
                lengthAttr.setValue(length);
                tSpaceObj.writeMetadata(lengthAttr);

            }

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
        final Hashtable<String, String> dTable = fileInfo.getDicomTable();
        final Enumeration<String> e = dTable.keys();

        final Hashtable<String, H5ScalarDS> groupTable = new Hashtable<String, H5ScalarDS>();

        // create nodes for each unique group
        while (e.hasMoreElements()) {
            final String tagKeyString = e.nextElement();
            final FileDicomKey tagKey = new FileDicomKey(tagKeyString);
            final String dicomTagValue = dTable.get(tagKeyString);
            if (groupTable.get(tagKey.getGroup()) == null) {
                final H5ScalarDS dicomGroupObject = (H5ScalarDS) fileFormat.createScalarDS(
                        FileMincHDF.DICOM_GROUP_PREFIX + tagKey.getGroup(), infoGroup, datatype, dims, maxdims, null,
                        0, null);
                groupTable.put(tagKey.getGroup(), dicomGroupObject);
            }

            final H5ScalarDS dGroupObj = groupTable.get(tagKey.getGroup());
            // add the attribute to the dicom group

            final String[] elementStr = new String[] {dicomTagValue};
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

        boolean is4D = false;
        if (image.is4DImage()) {
            is4D = true;
        }

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
                Preferences.debug("First switch(mDataType) not valid mDataType = " + mDataType + "\n", 
                		Preferences.DEBUG_FILEIO);
                return;
        }

        int torder = 0;
        if (image.getFileInfo()[0].getEndianess()) {
            torder = Datatype.ORDER_BE;
        } else {
            torder = Datatype.ORDER_LE;
        }

        final Datatype datatype = fileFormat.createDatatype(tclass, tsize, torder, tsign);

        final int beginSlice = options.getBeginSlice();
        final int endSlice = options.getEndSlice();
        int beginTime = 0;
        int endTime = 0;
        if (image.is4DImage()) {
            beginTime = options.getBeginTime();
            endTime = options.getEndTime();
        }

        long[] dims = new long[image.getExtents().length];
        long[] maxdims = new long[dims.length];
        if (dims.length == 2) {
            dims[0] = image.getExtents()[0];
            dims[1] = image.getExtents()[1];
            maxdims[0] = dims[0];
            maxdims[1] = dims[1];
        } else {
            if (is4D) {
                // order is t, z, y, x
                dims[0] = image.getExtents()[3];
                dims[1] = image.getExtents()[2];
                dims[2] = image.getExtents()[1];
                dims[3] = image.getExtents()[0];

                maxdims[0] = dims[0];
                maxdims[1] = dims[1];
                maxdims[2] = dims[2];
                maxdims[3] = dims[3];
            } else {
                // order is z, y, x
                dims[0] = (endSlice - beginSlice) + 1;
                dims[1] = image.getExtents()[1];
                dims[2] = image.getExtents()[0];

                maxdims[0] = dims[0];
                maxdims[1] = dims[1];
                maxdims[2] = dims[2];
            }

        }

        // create the Dataset (H5ScalarDS) that will hold the image data
        final H5ScalarDS imageObj = (H5ScalarDS) fileFormat.createScalarDS("image", imageNumGroup, datatype, dims,
                maxdims, null, 0, null);
        final DefaultMutableTreeNode imageDataNode = new DefaultMutableTreeNode(imageObj);
        model.insertNodeInto(imageDataNode, imageNumNode, imageNumNode.getChildCount());

        imageObj.init();

        // import the data
        final long[] start = imageObj.getStartDims(); // the starting dims

        // TODO: fix writing for hdf-java-2.7
        /*
         * final int[] selectedIndex = imageObj.getSelectedIndex(); final long[] selected = imageObj.getSelectedDims();
         * final String dimOrder = buildDimOrder(options.getAxisOrientation(), is4D); final String[] dimStrings =
         * dimOrder.split(",");
         */

        /*
         * selectedIndex[0] = 2; selected[0] = 26; selectedIndex[1] = 1; selected[1] = 512; selectedIndex[2] = 0;
         * selected[2] = 512;
         */

        /*
         * dimReorderIndexes = new int[dimStrings.length];
         * 
         * if (is4D) { for (int i = 0; i < dimStrings.length; i++) { if (dimStrings[i].equals(FileMincHDF.LEAF_X_SPACE)) {
         * dimReorderIndexes[i] = 2; } else if (dimStrings[i].equals(FileMincHDF.LEAF_Y_SPACE)) { dimReorderIndexes[i] =
         * 1; } else if (dimStrings[i].equals(FileMincHDF.LEAF_Z_SPACE)) { dimReorderIndexes[i] = 0; } else if
         * (dimStrings[i].equals(FileMincHDF.LEAF_T_SPACE)) { dimReorderIndexes[i] = 3; } } } else { for (int i = 0; i <
         * dimStrings.length; i++) { if (dimStrings[i].equals(FileMincHDF.LEAF_X_SPACE)) { dimReorderIndexes[i] = 2; }
         * else if (dimStrings[i].equals(FileMincHDF.LEAF_Y_SPACE)) { dimReorderIndexes[i] = 1; } else if
         * (dimStrings[i].equals(FileMincHDF.LEAF_Z_SPACE)) { dimReorderIndexes[i] = 0; } } }
         * 
         * for (int di = 0; di < dims.length; di++) { final int mipavOrder = getMipavOrderFromDimNode(dimStrings[di]);
         * selectedIndex[mipavOrder] = di; selected[mipavOrder] = dims[mipavOrder]; }
         * 
         * if ( !is4D) { // set the out-of-plane selection size to 1 selected[selectedIndex[2]] = 1; } else { // set the
         * time selection size to 1 selected[selectedIndex[3]] = 1; }
         */

        /* final int sliceSize = (int) (dims[0] * dims[1]); */

        final int sliceSize = image.getSliceSize();

        int numImages = 1;
        if (image.getExtents().length == 3) {
            // numImages = image.getExtents()[2];

            numImages = (endSlice - beginSlice) + 1;
        } else if (image.getExtents().length == 4) {
            numImages = image.getExtents()[2] * image.getExtents()[3];

            numImages = ( (endSlice - beginSlice) + 1) * ( (endTime - beginTime) + 1);
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
                Preferences.debug("Second switch(mDataType) not valid mDataType = " + mDataType + "\n", 
                		Preferences.DEBUG_FILEIO);
                return;
        }
        int sliceCounter = 0;
        int volCounter = 0;
        int counter = 0;
        int numVols = 0;
        if (is4D) {
            numVols = image.getExtents()[3];
        }

        // sometimes the minc2 file comes in with just a single min or max....in that case, we want to write it out like
        // that
        boolean hasImageMinMaxDimOrder = false;
        if (image.getFileInfo()[0].getFileFormat() == FileUtility.MINC_HDF) {
            hasImageMinMaxDimOrder = ((FileInfoMincHDF) image.getFileInfo()[0]).isHasImageMinMaxDimOrder();
        }

        double[] mins = new double[numImages];
        double[] maxs = new double[numImages];
        double[] intercepts = new double[numImages];
        double[] slopes = new double[numImages];

        if ( !hasImageMinMaxDimOrder) {
            mins = new double[1];
            maxs = new double[1];
            intercepts = new double[1];
            slopes = new double[1];
        }

        double vmin, vmax; // volume min and max

        if (image.getFileInfo()[0].getFileFormat() == FileUtility.MINC_HDF) {
            vmin = ((FileInfoMincHDF) (image.getFileInfo()[0])).getValidRange()[0];
            vmax = ((FileInfoMincHDF) (image.getFileInfo()[0])).getValidRange()[1];
        } else {
            vmin = image.getMin();
            vmax = image.getMax();
        }

        double slopeDivisor = vmax - vmin;

        if (slopeDivisor == 0) {
            slopeDivisor = 1;
        }

        // int jp;
        final float[] sliceData = new float[sliceSize];
        double smin, smax; // slice min and max

        int begin = 0;
        int end = 0;

        if (image.is3DImage()) {
            begin = beginSlice;
            end = endSlice;
        }
        if (image.is4DImage()) {
            begin = beginSlice * beginTime;
            end = endSlice * endTime;
        }

        if ( !hasImageMinMaxDimOrder) {
            if ( (image.getFileInfo()[0].getDataType() == ModelStorageBase.FLOAT)
                    || (image.getFileInfo()[0].getDataType() == ModelStorageBase.DOUBLE)) {
                slopes[0] = 1.0;
                intercepts[0] = 0.0;
                mins[0] = vmin;
                maxs[0] = vmax;
            } else {
                mins[0] = image.getMin();
                maxs[0] = image.getMax();

                slopes[0] = (maxs[0] - mins[0]) / slopeDivisor;

                intercepts[0] = mins[0] - (slopes[0] * vmin);
            }
        } else {
            for (int j = begin, jp = 0; j <= end; j++, jp++) {
                if ( (image.getFileInfo()[0].getDataType() == ModelStorageBase.FLOAT)
                        || (image.getFileInfo()[0].getDataType() == ModelStorageBase.DOUBLE)) {
                    slopes[jp] = 1.0;
                    intercepts[jp] = 0.0;
                    mins[jp] = vmin;
                    maxs[jp] = vmax;
                } else {

                    image.exportData(j * sliceSize, sliceSize, sliceData);
                    smin = Double.MAX_VALUE;
                    smax = -Double.MAX_VALUE;

                    // calculate min max values per slice
                    for (final float element : sliceData) {

                        if (element < smin) {
                            smin = element;
                        }

                        if (element > smax) {
                            smax = element;
                        }
                    }

                    mins[jp] = smin;
                    maxs[jp] = smax;

                    slopes[jp] = (smax - smin) / slopeDivisor;

                    intercepts[jp] = smin - (slopes[jp] * vmin);
                }
            }
        }

        float f;
        sliceCounter = 0;
        volCounter = 0;
        counter = 0;
        for (int j = begin, jp = 0; j <= end; j++, jp++) {

            if (is4D) {
                if (j % image.getExtents()[3] == 0) {
                    start[1] = sliceCounter;
                }
                if (volCounter >= numVols) {
                    volCounter = 0;
                }
                start[0] = volCounter;

            } else {
                start[0] = jp;
            }

            switch (mDataType) {
                case ModelStorageBase.UBYTE:
                case ModelStorageBase.BYTE:
                    if (is4D) {
                        image.exportData( (image.getExtents()[2] * sliceSize * volCounter) + (sliceSize * counter),
                                sliceSize, (byte[]) dataImportObj);
                        for (int k = 0; k < ((byte[]) dataImportObj).length; k++) {
                            if ( !hasImageMinMaxDimOrder) {
                                f = (float) ( ( ((byte[]) dataImportObj)[k] - intercepts[0]) / slopes[0]);
                            } else {
                                f = (float) ( ( ((byte[]) dataImportObj)[k] - intercepts[ (image.getExtents()[2] * volCounter)
                                        + counter]) / slopes[ (image.getExtents()[2] * volCounter) + counter]);
                            }
                            ((byte[]) dataImportObj)[k] = (byte) (Math.round(f));
                        }
                    } else {
                        image.exportData(j * sliceSize, sliceSize, (byte[]) dataImportObj);
                        for (int k = 0; k < ((byte[]) dataImportObj).length; k++) {
                            if ( !hasImageMinMaxDimOrder) {
                                f = (float) ( ( ((byte[]) dataImportObj)[k] - intercepts[0]) / slopes[0]);
                            } else {
                                f = (float) ( ( ((byte[]) dataImportObj)[k] - intercepts[j]) / slopes[j]);
                            }
                            ((byte[]) dataImportObj)[k] = (byte) (Math.round(f));
                        }
                    }
                    break;
                case ModelStorageBase.USHORT:
                case ModelStorageBase.SHORT:
                    if (is4D) {
                        image.exportData( (image.getExtents()[2] * sliceSize * volCounter) + (sliceSize * counter),
                                sliceSize, (short[]) dataImportObj);
                        for (int k = 0; k < ((short[]) dataImportObj).length; k++) {
                            if ( !hasImageMinMaxDimOrder) {
                                f = (float) ( ( ((short[]) dataImportObj)[k] - intercepts[0]) / slopes[0]);
                            } else {
                                f = (float) ( ( ((short[]) dataImportObj)[k] - intercepts[ (image.getExtents()[2] * volCounter)
                                        + counter]) / slopes[ (image.getExtents()[2] * volCounter) + counter]);
                            }
                            ((short[]) dataImportObj)[k] = (short) (Math.round(f));
                        }
                    } else {
                        image.exportData(j * sliceSize, sliceSize, (short[]) dataImportObj);
                        for (int k = 0; k < ((short[]) dataImportObj).length; k++) {
                            if ( !hasImageMinMaxDimOrder) {
                                f = (float) ( ( ((short[]) dataImportObj)[k] - intercepts[0]) / slopes[0]);
                            } else {
                                f = (float) ( ( ((short[]) dataImportObj)[k] - intercepts[jp]) / slopes[jp]);
                            }
                            ((short[]) dataImportObj)[k] = (short) (Math.round(f));
                        }
                    }
                    break;
                case ModelStorageBase.UINTEGER:
                case ModelStorageBase.INTEGER:
                    if (is4D) {
                        image.exportData( (image.getExtents()[2] * sliceSize * volCounter) + (sliceSize * counter),
                                sliceSize, (int[]) dataImportObj);
                        for (int k = 0; k < ((int[]) dataImportObj).length; k++) {
                            if ( !hasImageMinMaxDimOrder) {
                                f = (float) ( ( ((int[]) dataImportObj)[k] - intercepts[0]) / slopes[0]);
                            } else {
                                f = (float) ( ( ((int[]) dataImportObj)[k] - intercepts[ (image.getExtents()[2] * volCounter)
                                        + counter]) / slopes[ (image.getExtents()[2] * volCounter) + counter]);
                            }
                            ((int[]) dataImportObj)[k] = (Math.round(f));
                        }
                    } else {
                        image.exportData(j * sliceSize, sliceSize, (int[]) dataImportObj);
                        for (int k = 0; k < ((int[]) dataImportObj).length; k++) {
                            if ( !hasImageMinMaxDimOrder) {
                                f = (float) ( ( ((int[]) dataImportObj)[k] - intercepts[0]) / slopes[0]);
                            } else {
                                f = (float) ( ( ((int[]) dataImportObj)[k] - intercepts[j]) / slopes[j]);
                            }
                            ((int[]) dataImportObj)[k] = (Math.round(f));
                        }
                    }
                    break;
                case ModelStorageBase.FLOAT:
                    if (is4D) {
                        image.exportData( (image.getExtents()[2] * sliceSize * volCounter) + (sliceSize * counter),
                                sliceSize, (float[]) dataImportObj);
                        for (int k = 0; k < ((float[]) dataImportObj).length; k++) {
                            if ( !hasImageMinMaxDimOrder) {
                                f = (float) ( ( ((float[]) dataImportObj)[k] - intercepts[0]) / slopes[0]);
                            } else {
                                f = (float) ( ( ((float[]) dataImportObj)[k] - intercepts[ (image.getExtents()[2] * volCounter)
                                        + counter]) / slopes[ (image.getExtents()[2] * volCounter) + counter]);
                            }
                            ((float[]) dataImportObj)[k] = f;
                        }
                    } else {
                        image.exportData(j * sliceSize, sliceSize, (float[]) dataImportObj);
                        for (int k = 0; k < ((float[]) dataImportObj).length; k++) {
                            if ( !hasImageMinMaxDimOrder) {
                                f = (float) ( ( ((float[]) dataImportObj)[k] - intercepts[0]) / slopes[0]);
                            } else {
                                f = (float) ( ( ((float[]) dataImportObj)[k] - intercepts[j]) / slopes[j]);
                            }
                            ((float[]) dataImportObj)[k] = f;
                        }
                    }
                    break;
                case ModelStorageBase.DOUBLE:
                    if (is4D) {
                        image.exportData( (image.getExtents()[2] * sliceSize * volCounter) + (sliceSize * counter),
                                sliceSize, (double[]) dataImportObj);
                        for (int k = 0; k < ((double[]) dataImportObj).length; k++) {
                            if ( !hasImageMinMaxDimOrder) {
                                f = (float) ( ( ((double[]) dataImportObj)[k] - intercepts[0]) / slopes[0]);
                            } else {
                                f = (float) ( ( ((double[]) dataImportObj)[k] - intercepts[ (image.getExtents()[2] * volCounter)
                                        + counter]) / slopes[ (image.getExtents()[2] * volCounter) + counter]);
                            }
                            ((double[]) dataImportObj)[k] = f;
                        }
                    } else {
                        image.exportData(j * sliceSize, sliceSize, (double[]) dataImportObj);
                        for (int k = 0; k < ((double[]) dataImportObj).length; k++) {
                            if ( !hasImageMinMaxDimOrder) {
                                f = (float) ( ( ((double[]) dataImportObj)[k] - intercepts[0]) / slopes[0]);
                            } else {
                                f = (float) ( ( ((double[]) dataImportObj)[k] - intercepts[j]) / slopes[j]);
                            }
                            ((double[]) dataImportObj)[k] = (f);
                        }
                    }
                    break;
                default:
                    System.err.println("Third switch(mDataType) not valid yet mDataType = " + mDataType);
                    Preferences.debug("j = " + j + "\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("Third switch(mDataType) not valid mDataType = " + mDataType + "\n", 
                    		Preferences.DEBUG_FILEIO);
                    return;
            }
            imageObj.write(dataImportObj);
            if (is4D) {
                volCounter++;
                if (j != 0 && j % numVols == 0) {
                    counter++;
                }
                if (j % image.getExtents()[3] == 0) {
                    sliceCounter++;
                }
            }
            fireProgressStateChanged(Math.round(5 + ((float) j / numImages) * 95));
        }

        // dimorder based on orientation
        // orientation of the image.
        String dimOrder = new String();
        if (is4D) {
            dimOrder = "time,";
        }
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
        if (is4D) {
            dims = new long[2];
            maxdims = new long[2];
        } else {
            dims = new long[1];
            maxdims = new long[1];
        }

        // int numSlices = 1;
        dims[0] = 1;
        if (image.getExtents().length == 3) {
            dims[0] = image.getExtents()[2];
            maxdims[0] = dims[0];
            // numSlices = image.getExtents()[2];
        } else if (image.getExtents().length == 4) {
            dims[0] = image.getExtents()[3];
            dims[1] = image.getExtents()[2];
            maxdims[0] = dims[0];
            maxdims[1] = dims[1];
            // numSlices = image.getExtents()[2] * image.getExtents()[3];
        }

        if ( !hasImageMinMaxDimOrder) {
            dims[0] = 1;
        }

        final H5ScalarDS imageMaxObj = (H5ScalarDS) fileFormat.createScalarDS("image-max", imageNumGroup,
                imageRangeDatatype, dims, maxdims, null, 0, null);
        final DefaultMutableTreeNode imageMaxNode = new DefaultMutableTreeNode(imageMaxObj);
        model.insertNodeInto(imageMaxNode, imageNumNode, imageNumNode.getChildCount());

        final H5ScalarDS imageMinObj = (H5ScalarDS) fileFormat.createScalarDS("image-min", imageNumGroup,
                imageRangeDatatype, dims, maxdims, null, 0, null);
        final DefaultMutableTreeNode imageMinNode = new DefaultMutableTreeNode(imageMinObj);
        model.insertNodeInto(imageMinNode, imageNumNode, imageNumNode.getChildCount());

        // double[] imageMax = new double[numSlices];
        // double[] imageMin = new double[numSlices];

        // final double[] imageSliceBuffer = new double[image.getSliceSize()];
        sliceCounter = 0;
        volCounter = 0;
        counter = 0;
        if (is4D) {
            numVols = image.getExtents()[3];
        }

        // init and then read in the two arrays
        imageMaxObj.init();
        imageMinObj.init();

        imageMaxObj.write(maxs);
        imageMinObj.write(mins);

        // write the dimorder attribute to the image-max and image-min nodes
        if (hasImageMinMaxDimOrder) {
            final String[] rangeDimString = new String[] {"zspace"};
            if (is4D) {
                rangeDimString[0] = "time,zspace";
            }
            dType = fileFormat.createDatatype(Datatype.CLASS_STRING, rangeDimString[0].length() + 1, Datatype.NATIVE,
                    -1);
            attrDims[0] = 1;
            attr = new Attribute("dimorder", dType, attrDims);
            attr.setValue(rangeDimString);
        }

        final String[] varTypeMMString = new String[] {"var_attribute"};
        dType = fileFormat.createDatatype(Datatype.CLASS_STRING, varTypeMMString[0].length() + 1, Datatype.NATIVE, -1);
        attrDims[0] = 1;
        final Attribute varTypeAttr = new Attribute("vartype", dType, attrDims);
        varTypeAttr.setValue(varTypeMMString);
        if (hasImageMinMaxDimOrder) {
            imageMaxObj.writeMetadata(attr);
        }
        imageMaxObj.writeMetadata(varIDAttr);
        imageMaxObj.writeMetadata(varTypeAttr);
        imageMaxObj.writeMetadata(versionAttr);

        if (hasImageMinMaxDimOrder) {
            imageMinObj.writeMetadata(attr);
        }
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

        Preferences.debug("Entering writeImage\n", Preferences.DEBUG_FILEIO);
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

        buildDimensionNode(image, options, format, mincNode, model);

        buildInfoNode(image, format, mincNode, model);

        Preferences.debug("Entering buildImageNode\n", Preferences.DEBUG_FILEIO);
        buildImageNode(image, options, format, mincNode, model);

        h5File.close();
    }

    /**
     * Gets the proper MIPAV dimension ordering index for a given Minc dim node string using the image info.
     * 
     * @param dimNodeStr A minc dimension node string (i.e., xspace, yspace, zspace, time).
     * @return The proper MIPAV dimension ordering index.
     */
    private int getMipavOrderFromDimNode(final String dimNodeStr) {
        int order = -1;

        if (is4D) {

            if (dimNodeStr.equals(FileMincHDF.LEAF_X_SPACE)) {
                for (int k = 0; k < dimReorderIndexes.length; k++) {
                    if (dimReorderIndexes[k] == 2) {
                        order = k;
                        break;
                    }
                }

            } else if (dimNodeStr.equals(FileMincHDF.LEAF_Y_SPACE)) {
                for (int k = 0; k < dimReorderIndexes.length; k++) {
                    if (dimReorderIndexes[k] == 1) {
                        order = k;
                        break;
                    }
                }

            } else if (dimNodeStr.equals(FileMincHDF.LEAF_Z_SPACE)) {
                for (int k = 0; k < dimReorderIndexes.length; k++) {
                    if (dimReorderIndexes[k] == 0) {
                        order = k;
                        break;
                    }
                }
            } else if (dimNodeStr.equals(FileMincHDF.LEAF_T_SPACE)) {
                for (int k = 0; k < dimReorderIndexes.length; k++) {
                    if (dimReorderIndexes[k] == 3) {
                        order = k;
                        break;
                    }
                }
            }
        } else {

            if (dimNodeStr.equals(FileMincHDF.LEAF_X_SPACE)) {
                for (int k = 0; k < dimReorderIndexes.length; k++) {
                    if (dimReorderIndexes[k] == 2) {
                        order = k;
                        break;
                    }
                }

            } else if (dimNodeStr.equals(FileMincHDF.LEAF_Y_SPACE)) {
                for (int k = 0; k < dimReorderIndexes.length; k++) {
                    if (dimReorderIndexes[k] == 1) {
                        order = k;
                        break;
                    }
                }

            } else if (dimNodeStr.equals(FileMincHDF.LEAF_Z_SPACE)) {
                for (int k = 0; k < dimReorderIndexes.length; k++) {
                    if (dimReorderIndexes[k] == 0) {
                        order = k;
                        break;
                    }
                }
            }
        }

        return order;
    }
    
    /**
     * Return the proper unit based on a given unit string (abbrevation or full word).
     * @param val The unit name or abbreviation.
     * @return The proper unit for the given string, or Unit.UNKNOWN_MEASURE if not recognized.
     */
    public static final Unit getUnitFromString(String val) {
        Unit u = Unit.getUnit(val);
        if (u == Unit.UNKNOWN_MEASURE) {
            u = Unit.getUnitFromAbbrev(val);
            
            if (u == Unit.UNKNOWN_MEASURE) {
                // some minc files use 's' for seconds instead of the 'sec' that we use
                if (val.equalsIgnoreCase("s")) {
                    u = Unit.SECONDS;
                }
            }
        }
        
        return u;
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
