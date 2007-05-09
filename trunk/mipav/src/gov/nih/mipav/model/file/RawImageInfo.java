package gov.nih.mipav.model.file;


public class RawImageInfo {
	
	private int type;
	private int [] dims;
	private float [] res;
	private int [] units;
	private int offset;
	private boolean bigEndian;
	
	public RawImageInfo(int type, int [] dims, float [] res, int [] units, int offset, boolean bigEndian) {
		this.type = type;
		this.dims = dims;
		this.res = res;
		this.units = units;
		this.offset = offset;
		this.bigEndian = bigEndian;
	}
	
	public int getDataType() {
		return this.type;
	}
	
	public int[] getExtents() {
		return this.dims;
	}
	
	public int[] getUnitsOfMeasure() {
		return this.units;
	}
	
	public float [] getResolutions() {
		return this.res;
	}
	
	public boolean getEndianess() {
		return this.bigEndian;
	}
	
	public int getOffset() {
		return this.offset;
	}
}
