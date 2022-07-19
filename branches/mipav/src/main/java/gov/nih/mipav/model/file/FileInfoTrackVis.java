package gov.nih.mipav.model.file;

import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogText;

public class FileInfoTrackVis extends FileInfoBase {

	//~ Static fields/initializers -------------------------------------------------------------------------------------
	
	//~ Instance fields ------------------------------------------------------------------------------------------------

	/** Number of scalars per track point and number of properties per track */
	private int numScalar, numProp;
	
	/** Scalar names and property names */
	private String[] scalarNames, propNames;
	
	/** Properties per track */
	private float[][] properties;
	
	/** Reserved for future TrackVis use */
	private String reserved;
	
	/** Storing order of image data */
	private String voxelOrder;
	
	/** Paddings */
	private String pad2, pad1;
	
	/** Internal trakcVis use */
	private String invertXStr, invertYStr, invertZStr;
	
	/** Internal TrackVis use */
	private String swapXY, swapYZ, swapXZ;
	
	/** Number of tracts */
	private int numTracks;
	
	/** Version number, current is 2 */
	private int version;
	
	/** Size of header, should be 1000 */
	private int hdrSize;
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * File info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoTrackVis(String name, String directory, int format) {
        super(name, directory, format);
    }

  //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Displays the file information.
     *
     * @param  dlog    dialog box that is written to
     * @param  matrix  transformation matrix
     */
	public void displayAboutInfo(JDialogBase dLog, TransMatrix matrix) {
		JDialogText dialog = (JDialogText) dLog;
		
		displayPrimaryInfo(dialog, matrix);
        dialog.append("\n\n                Other information\n\n");
	}

	public int getNumScalar() {
		return numScalar;
	}

	public int getNumProp() {
		return numProp;
	}

	public String[] getScalarNames() {
		return scalarNames;
	}

	public String[] getPropNames() {
		return propNames;
	}

	public String getReserved() {
		return reserved;
	}

	public String getVoxelOrder() {
		return voxelOrder;
	}

	public String getPad2() {
		return pad2;
	}

	public String getPad1() {
		return pad1;
	}

	public String getInvertXStr() {
		return invertXStr;
	}

	public String getInvertYStr() {
		return invertYStr;
	}

	public String getInvertZStr() {
		return invertZStr;
	}

	public String getSwapXY() {
		return swapXY;
	}

	public String getSwapYZ() {
		return swapYZ;
	}

	public String getSwapXZ() {
		return swapXZ;
	}

	public int getNumTracts() {
		return numTracks;
	}

	public int getVersion() {
		return version;
	}

	public int getHdrSize() {
		return hdrSize;
	}

	public float[] getTrackProperties(int i) {
		return properties[i];
	}
	
	public float[][] getProperties() {
		return properties;
	}

	public void setProperties(float[][] properties) {
		this.properties = properties;
	}

	public void setNumScalar(int numScalar) {
		this.numScalar = numScalar;
	}

	public void setNumProp(int numProp) {
		this.numProp = numProp;
	}

	public void setScalarNames(String[] scalarNames) {
		this.scalarNames = scalarNames;
	}

	public void setPropNames(String[] propNames) {
		this.propNames = propNames;
	}

	public void setReserved(String reserved) {
		this.reserved = reserved;
	}

	public void setVoxelOrder(String voxelOrder) {
		this.voxelOrder = voxelOrder;
	}

	public void setPad2(String pad2) {
		this.pad2 = pad2;
	}

	public void setPad1(String pad1) {
		this.pad1 = pad1;
	}

	public void setInvertXStr(String invertXStr) {
		this.invertXStr = invertXStr;
	}

	public void setInvertYStr(String invertYStr) {
		this.invertYStr = invertYStr;
	}

	public void setInvertZStr(String invertZStr) {
		this.invertZStr = invertZStr;
	}

	public void setSwapXY(String swapXY) {
		this.swapXY = swapXY;
	}

	public void setSwapYZ(String swapYZ) {
		this.swapYZ = swapYZ;
	}

	public void setSwapZX(String swapXZ) {
		this.swapXZ = swapXZ;
	}

	public void setNumTracks(int numTracks) {
		this.numTracks = numTracks;
	}

	public void setVersion(int version) {
		this.version = version;
	}

	public void setHdrSize(int hdrSize) {
		this.hdrSize = hdrSize;
	}

}
