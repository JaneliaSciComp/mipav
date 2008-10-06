package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.util.*;

import javax.swing.tree.*;

import ncsa.hdf.object.Attribute;
import ncsa.hdf.object.HObject;
import gov.nih.mipav.model.file.HDF.*;


public class FileInfoMincHDF extends FileInfoBase {
	/**
     * 
     */
    private static final long serialVersionUID = 1372818551072422068L;
    
    /**
     * The dimension tree node that holds xspace, yspace, and (optionally) zspace nodes
     */
    private transient DefaultMutableTreeNode dimensionNode;


    /**
     * The information node that holds acquisition information, dicom tags, and more
     */
    private transient DefaultMutableTreeNode informationNode;
    
    /**
     * The image node that holds information about the image data (rescaling, dimorder, actual data).
     */
    private transient DefaultMutableTreeNode imageNode;

    /**
     * The axis orientation static types
     */
    protected static int[] axisOrientation = { ORI_UNKNOWN_TYPE, ORI_UNKNOWN_TYPE, ORI_UNKNOWN_TYPE };

    /**
     * The valid range for image pixel values
     */
    private double [] validRange = null;

    /**
     * Hashtable to hold the tags for conversion to->from dicom and to-> mipav XML format
     */
    private transient Hashtable<FileDicomKey,FileDicomTag> dicomTable = new Hashtable<FileDicomKey,FileDicomTag>();

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * MINC file information constructor.
     *
     * @param  name       file name
     * @param  directory  file directory
     * @param  format     format (in this case, MINC)
     */
    public FileInfoMincHDF(String name, String directory, int format) {
	super(name, directory, format);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Returns all of the dicom-converted tags in the minc header as a tag-value hashtable.
     *
     * @return  a tag-value hashtable
     */
    public Hashtable<String,String> convertTagsToTable() {
	// create the dicom table so that it has <String><String>
	Hashtable<String,String> dicomTagStrings = new Hashtable<String,String>();

	if (informationNode != null) {
	    DefaultMutableTreeNode currentNode;
	    String group;
	    String element;
	    for (int i = 0; i < informationNode.getChildCount(); i++) {
		currentNode = (DefaultMutableTreeNode)informationNode.getChildAt(i);
		if (currentNode.getUserObject().toString().startsWith(FileMincHDF.DICOM_GROUP_PREFIX)) {
		    group = currentNode.getUserObject().toString().substring(FileMincHDF.DICOM_GROUP_PREFIX.length());
		    try {
			Iterator<Attribute> it = ((HObject)currentNode.getUserObject()).getMetadata().iterator();
			while(it.hasNext()) {
			    Attribute attr = it.next();
			    element = attr.getName().substring(FileMincHDF.DICOM_ELEMENT_PREFIX.length());

			    dicomTagStrings.put("(" + group.toUpperCase() + "," + element.toUpperCase() + ")", 
				    ((String[])attr.getValue())[0].trim());
			}
		    } catch (Exception e) {
			e.printStackTrace();
			continue;
		    }
		}
	    }
	}

	return dicomTagStrings;
    }

    /**
     * Displays important information about the image.
     *
     * @param  dlog    where to display the info
     * @param  matrix  the transformation matrix
     */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix) {
	// never called
    }

    /**
     * Sets the dimension node (xspace, yspace and optional zspace nodes)
     * @param dimNode the dimension node
     */
    public void setDimensionNode(DefaultMutableTreeNode dimNode) {
	this.dimensionNode = dimNode;
    }

    /**
     * Returns the dimension node (only 1 exists at fileinfo[0] position
     * @return the dimension node
     */
    public DefaultMutableTreeNode getDimensionNode() {
	return this.dimensionNode;
    }

    /**
     * Sets the information node (includes dicom tags and patient info
     * @param infoNode the information node
     */
    public void setInfoNode(DefaultMutableTreeNode infoNode) {
	this.informationNode = infoNode;
    }

    /**
     * Returns the information node with dicom/patient info
     * @return the information node
     */
    public DefaultMutableTreeNode getInfoNode() {
	return this.informationNode;
    }
    
    /**
     * Sets the image node (includes image info and data)
     * @param imageNode the image node
     */
    public void setImageNode(DefaultMutableTreeNode imageNode) {
	this.imageNode = imageNode;
    }

    /**
     * Returns the image node (image info and data)
     * @return the image node
     */
    public DefaultMutableTreeNode getImageNode() {
	return this.imageNode;
    }

    /**
     * Sets the image modality based on the string
     */
    public void setModality(String modality) {

	if (modality.equals("PET__")) {
	    setModality(FileInfoBase.POSITRON_EMISSION_TOMOGRAPHY);
	} else if (modality.equals("MRI__")) {
	    setModality(FileInfoBase.MAGNETIC_RESONANCE);
	} else if (modality.equals("SPECT")) {
	    setModality(FileInfoBase.SINGLE_PHOTON_EMISSION_COMPUTED_TOMOGRAPHY);
	} else if (modality.equals("GAMMA")) {
	    // setModality(FileInfoBase.);
	} else if (modality.equals("MRS__")) {
	    setModality(FileInfoBase.MAGNETIC_RESONANCE_SPECTROSCOPY);
	} else if (modality.equals("MRA__")) {
	    setModality(FileInfoBase.MAGNETIC_RESONANCE_ANGIOGRAPHY);
	} else if (modality.equals("CT___")) {
	    setModality(FileInfoBase.COMPUTED_TOMOGRAPHY);
	} else if (modality.equals("DSA__")) {
	    // setModality(FileInfoBase.);
	} else if (modality.equals("DR___")) {
	    setModality(FileInfoBase.DIGITAL_RADIOGRAPHY);
	}
    }

    /**
     * Sets the valid range for the image data
     * @param valid_range double valid range of image data values
     */
    public void setValidRange(double[] valid_range) {
	this.validRange = valid_range;
    }

    /**
     * Gets the valid range for image data values
     * @return the valid range (min to max) array
     */
    public double [] getValidRange() {
	return this.validRange;
    }

    /**
     * Sets the dicom converted tag hashtable
     * @param dTable hashtable holding dicom keys and tags
     */
    public void setDicomTable(Hashtable<FileDicomKey,FileDicomTag> dTable) {
	this.dicomTable = dTable;
    }

    /**
     * Retrieves the dicom converted tag hashtable
     * @return the dicom hashtable
     */
    public Hashtable<FileDicomKey,FileDicomTag> getDicomTable() {
	return this.dicomTable;
    }

    /**
     * Strips out the relevant patient info (if present) to insert into MIPAV's XML header
     * @param dInfo destination XML fileinfo
     */
    public void convertPatientInfo(FileInfoImageXML dInfo) {
	if (informationNode == null) {
	    return;
	}
	
	//System.err.println("convertPatientInfo");
	
	int children = informationNode.getChildCount();
	DefaultMutableTreeNode currentNode;
	for (int i = 0; i < children; i++) {
	    currentNode = (DefaultMutableTreeNode)informationNode.getChildAt(i);
	    if (currentNode.getUserObject().toString().equals(FileMincHDF.LEAF_PATIENT)) {

		try {
		    List<Attribute> metaData = ((HObject)currentNode.getUserObject()).getMetadata();
		    Iterator<Attribute> it = metaData.iterator();
		    while(it.hasNext()) {

			Attribute currentAttribute = it.next();
			String name = currentAttribute.getName();
			if (name.equals("full_name")) {
			    dInfo.setSubjectName(((String[])currentAttribute.getValue())[0]);
			} else if (name.equals("sex")) {
			    String mincSex = ((String[])currentAttribute.getValue())[0];
			    
			    if (mincSex.equalsIgnoreCase("male__")) {
				dInfo.setSex("Male");
			    } else if (mincSex.equalsIgnoreCase("female")) {
				dInfo.setSex("Female");
			    } else if (mincSex.equalsIgnoreCase("other_")) {
				dInfo.setSex("Other");
			    } else {
				dInfo.setSex("Other");
			    }
			} else if (name.equals("identification")) {
			    dInfo.setSubjectID(((String[])currentAttribute.getValue())[0]);
			} else if (name.equals("birthdate")) {
			    String mincDOB = ((String[])currentAttribute.getValue())[0];
			    dInfo.setDOB(mincDOB.substring(0, 4) + "-" + mincDOB.substring(4, 6) + "-" + mincDOB.substring(6, 8));
			}
		    }
		} catch (Exception e) {
		    e.printStackTrace();
		    return;
		}
	    }
	}
    }

    /**
     * Converts from the minc start location to the dicom start locations 
     * @param step the resolutions (step, can be negative)
     * @param cosines the directional matrix
     * @param isCentered is the axis centered
     * @param slice the slice number
     * @param mincStartLoc the original minc start location [x,y, opt z]
     * @return
     */
    public final double[] getConvertStartLocationsToDICOM(double[] step, double[][]cosines, 
	    boolean[]isCentered, int slice, double [] mincStartLoc) {

    	double[] startLocs = new double[getExtents().length];

	if (startLocs.length == 2) {
	    startLocs[0] = mincStartLoc[0];
	    startLocs[1] = mincStartLoc[1];
	} else {
	    startLocs[0] = mincStartLoc[0];
	    startLocs[1] = mincStartLoc[1];
	    startLocs[2] = mincStartLoc[2] + (step[2] * slice);
	}

	TransMatrix matrix = new TransMatrix(getExtents().length + 1);
	matrix.MakeIdentity();

	for (int i = 0; i < 3; i++) {
	    for (int j = 0; j < cosines[i].length; j++) {
		matrix.set(i, j, cosines[i][j]);
	    }

	}

	matrix.Inverse();

	double[] transformedPt = new double[getExtents().length];

	if (isCentered[0]) {
	    startLocs[0] -= (step[0] / 2);
	}

	if (isCentered[1]) {
	    startLocs[1] -= (step[1] / 2);
	}

	// mni seems not to adjust the zstart by the zstep even when xspace has the attrib alignment=centre
	/*if (isZCentered) {
	 *  startLocs[2] -= (step[2] / 2);}*/

	if (getExtents().length == 2) {
	    matrix.transform(startLocs[0], startLocs[1], transformedPt);
	} else if (getExtents().length == 3) {
	    matrix.transform(startLocs[0], startLocs[1], startLocs[2], transformedPt);
	}
	
	
	//Appears that this transformed Pt...is in the following order....xspace, yspace, zspace
	//in minc, xspace cooresponds to l/r, yspace corresponds to a/p, zspace corresponds to i/s
	//get the transformed point in the right mipav space order
	
	double[] transformedPtReordered = new double[getExtents().length];

	if (getImageOrientation() == FileInfoBase.SAGITTAL) {
		//saggital is a/p, i/s, l/r in mipav space
		transformedPtReordered[0] = transformedPt[1];
		transformedPtReordered[1] = transformedPt[2];
		transformedPtReordered[2] = transformedPt[0];
	}else if (getImageOrientation() == FileInfoBase.AXIAL) {
		//axial is l/r, a/p, i/s in mipav space
		transformedPtReordered[0] = transformedPt[0];
		transformedPtReordered[1] = transformedPt[1];
		transformedPtReordered[2] = transformedPt[2];
	}else if (getImageOrientation() == FileInfoBase.CORONAL) {
		//coronal is l/r, i/s, a/p in mipav space
		transformedPtReordered[0] = transformedPt[0];
		transformedPtReordered[1] = transformedPt[2];
		transformedPtReordered[2] = transformedPt[1];
	}

	
	if (startLocs.length == 3) {

	    if (getImageOrientation() == FileInfoBase.SAGITTAL) {
	    	transformedPtReordered[0] = -transformedPtReordered[0];
	    	transformedPtReordered[2] = -transformedPtReordered[2];
	    } else if (getImageOrientation() == FileInfoBase.AXIAL) {
	    	transformedPtReordered[0] = -transformedPtReordered[0];
	    	transformedPtReordered[1] = -transformedPtReordered[1];
	    } else if (getImageOrientation() == FileInfoBase.CORONAL) {
	    	transformedPtReordered[0] = -transformedPtReordered[0];
	    	transformedPtReordered[2] = -transformedPtReordered[2];
	    }
	    
	    transformedPt = transformedPtReordered;
	}

	// System.err.println("convert: result[" + slice + "]:\t" + transformedPt[0] + " " + transformedPt[1] + " " +
	//   transformedPt[2]);

	return transformedPt;
    }

    /**
     * Sets start locations of each axis.
     *
     * @param  origin  the image origin
     */
    public final void setStartLocations(double[] origin) {

	if (origin.length != 3) {
	    Preferences.debug("Start locations array must be of length 3.\n");

	    return;
	}

	float[] fOrigin = new float[origin.length];

	for (int i = 0; i < origin.length; i++) {
	    fOrigin[i] = (float) origin[i];
	}

	super.setOrigin(fOrigin);
    }

    /**
     * In MINC images, "real" values for pixels are calculated by taking the given image min and image max and rescaling
     * the data accordingly. Image min and image max are given per slice.
     * @param rescaleIntercept
     * @param rescaleSlope
     * @param imageMax
     * @param imageMin
     * @param validMax
     * @param validMin
     */
    public static void calculateRescaleIntercept(double[] rescaleIntercept, double[] rescaleSlope, 
	    double[] imageMax, double [] imageMin,
	    double [] valid_range) {

	try {

	    for (int i = 0; i < rescaleSlope.length; i++) {
		rescaleSlope[i] = FileInfoMinc.calculateSlope(imageMax[i], imageMin[i], valid_range[1], valid_range[0]);
		rescaleIntercept[i] = FileInfoMinc.calculateIntercept(imageMin[i], rescaleSlope[i], valid_range[0]);
	    }
	} catch (ArrayIndexOutOfBoundsException error) {
	    
	    for (int i = 0; i < rescaleSlope.length; i++) {
		rescaleSlope[i] = 1.0;
		rescaleIntercept[i] = 0.0;
	    }
	}
    }
}
